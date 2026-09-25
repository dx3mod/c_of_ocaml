open Containers
module Code = Js_of_ocaml_compiler.Code

module Closure = struct
  type info = {
    free_variables : Code.Var.t list;
    continues : Code.cont;
    parameters : Code.Var.t list;
  }
end

module String_interner = CCHashSet.Make (String)

module Context = struct
  type t = {
    program : Code.program;
    closures : closures;
    string_interner : String_interner.t;
  }

  and closures = (Code.Addr.t, Closure.info) Hashtbl.t

  let find_closures program : closures =
    let free_vars = Js_of_ocaml_compiler.Freevars.f program in

    let find_closure parameters ((label_address, _) as continues) =
      let free_variables =
        Code.Addr.Map.find_opt label_address free_vars
        |> Option.map_or ~default:[] Code.Var.Set.elements
      in

      (label_address, Closure.{ free_variables; parameters; continues })
    in

    Code.fold_closures program
      (fun _ parameters continues acc ->
        find_closure parameters continues :: acc)
      []
    |> Hashtbl.of_list

  let make program =
    let closures = find_closures program in
    let string_interner = String_interner.create 100 in
    { program; closures; string_interner }

  let find_variables_at_block context label_address =
    let variables = Dynarray.create () in

    let fold = Code.fold_children in
    Code.traverse { fold }
      begin fun label_address () ->
        let block = Code.Addr.Map.find label_address context.program.blocks in

        Dynarray.append_list variables block.params;
        Dynarray.append_list variables
        @@ List.filter_map
             (function Code.Let (variable, _), _ -> Some variable | _ -> None)
             block.Code.body;

        match fst block.branch with
        | Pushtrap (_, exn_var, _) -> Dynarray.add_last variables exn_var
        | _ -> ()
      end
      label_address context.program.blocks ();

    Dynarray.to_list variables
end

module Stack_frame = struct
  type t = {
    variables : (Code.Var.t * slot, Vector.rw) Vector.t;
    free_slots : int Dynarray.t;
    mutable current_slot : int;
  }

  and slot = int

  let create () =
    {
      variables = Vector.create ();
      free_slots = Dynarray.create ();
      current_slot = 0;
    }

  let find_variable_slot_opt { variables; _ } variable =
    Vector.find_map
      (fun (v, slot) -> if Code.Var.equal v variable then Some slot else None)
      variables

  let find_variable_slot frame variable =
    find_variable_slot_opt frame variable
    |> Option.get_exn_or "not found variable at stack frame"

  let add_variable frame variable =
    if
      not
      @@ Vector.exists (fun (v, _) -> Code.Var.equal v variable) frame.variables
    then
      let slot =
        Dynarray.pop_last_opt frame.free_slots
        |> Option.get_lazy @@ fun () ->
           let slot = frame.current_slot in
           frame.current_slot <- succ frame.current_slot;
           slot
      in

      Vector.push frame.variables (variable, slot)

  let add_variables frame variables = List.iter (add_variable frame) variables

  let remove_variable frame variable =
    Vector.findi (fun (v, _) -> Code.Var.equal v variable) frame.variables
    |> Option.iter @@ fun (index, (_, slot)) ->
       Dynarray.add_last frame.free_slots slot;
       Vector.remove_unordered frame.variables index

  let count_variables { variables; _ } = Vector.length variables

  let variable_not_exist { variables; _ } variable =
    not @@ Vector.exists (fun (v, _) -> Code.Var.equal variable v) variables
end

module Cir_program = struct
  type t = Cir.instruction Dynarray.t

  let create : unit -> t = Dynarray.create
  let add : t -> Cir.instruction -> unit = Dynarray.add_last
  let to_list : t -> _ list = Dynarray.to_list
  let pp ppf instars = Dynarray.iter (Cir.pp_instruction ppf) instars
end

let integer_binary_operations =
  [
    ("%int_add", "+");
    ("%int_sub", "-");
    ("%int_mul", "*");
    ("%int_div", "/");
    ("%int_mod", "%");
    ("%direct_int_mul", "*");
    ("%direct_int_div", "/");
    ("%direct_int_mod", "%");
    ("%int_and", "&");
    ("%int_or", "|");
    ("%int_xor", "^");
    ("%int_lsl", "<<");
    ("%int_asr", ">>");
  ]

let rec compile_closure cir context label_address closure_info =
  let stack_frame = Stack_frame.create () in

  Context.find_variables_at_block context label_address
  |> Stack_frame.add_variables stack_frame;

  let closure_variables =
    closure_info.Closure.free_variables @ closure_info.parameters
  in

  List.iter
    begin fun variable ->
      if Stack_frame.variable_not_exist stack_frame variable then
        Stack_frame.add_variable stack_frame variable
    end
    closure_variables;

  let required_stack_size = Stack_frame.count_variables stack_frame in
  let cir_closure = Cir_program.create () in
  begin
    Cir_program.add cir_closure (Cir.Reserve_stack_size required_stack_size);

    List.iteri
      begin fun env_index variable ->
        let slot = Stack_frame.find_variable_slot stack_frame variable in

        Cir_program.add cir_closure
        @@ Cir.Set_stack_frame_variable
             (slot, Constanta (Raw_c (Printf.sprintf "env[%d]" env_index)))
      end
      closure_variables;

    compile_environment_assigns cir_closure context stack_frame label_address
      (snd closure_info.continues);

    compile_block cir_closure context stack_frame
      Dynarray.(create ())
      label_address
  end;

  Cir_program.add cir
    Cir.(
      Closure_definition
        (Printf.sprintf "c%d" label_address, Cir_program.to_list cir_closure))

and compile_environment_assigns =
  let rename_id = ref 0 in

  fun cir context stack_frame label_address arguments ->
    let id = !rename_id in
    incr rename_id;

    let t i = Printf.sprintf "t%d_i%d" id i in

    let variables_at_block =
      (Code.Addr.Map.find label_address context.Context.program.blocks).params
    in

    List.iteri
      (fun i arg_variable ->
        let expression =
          match Stack_frame.find_variable_slot_opt stack_frame arg_variable with
          | Some slot -> Cir.Get_stack_frame_variable slot
          | None -> Cir.Get_variable arg_variable
        in

        let definition = Cir.Variable_definition (t i, expression) in

        Cir_program.add cir (Variable_declaration (t i));
        Cir_program.add cir definition)
      arguments;

    List.iteri
      (fun i param_variable ->
        let expression = Cir.Constanta (Raw_c (t i)) in

        let instruction =
          match
            Stack_frame.find_variable_slot_opt stack_frame param_variable
          with
          | Some slot -> Cir.Set_stack_frame_variable (slot, expression)
          | None -> Cir.Set_variable (param_variable, expression)
        in

        Cir_program.add cir instruction)
      variables_at_block

and compile_block cir context stack_frame already_visited label_address =
  if not @@ Dynarray.exists (( = ) label_address) already_visited then begin
    Dynarray.add_last already_visited label_address;

    Cir_program.add cir (Label label_address);

    let block =
      Code.Addr.Map.find label_address context.Context.program.blocks
    in

    compile_block_body cir context stack_frame block.Code.body;
    compile_branch cir context stack_frame already_visited
      (fst block.Code.branch)
  end

and compile_block_body cir context stack_frame body =
  let take_closures instructions =
    List.take_while
      (function Code.Let (_, Code.Closure _), _ -> true | _ -> false)
      instructions
  in

  let rec aux = function
    | [] -> ()
    | instructions -> begin
        let closures_instructions = take_closures instructions in
        let instructions =
          List.drop (List.length closures_instructions) instructions
        in

        begin match (closures_instructions, instructions) with
        | [], [] -> ()
        | [], instruction :: instructions ->
            compile_instruction cir context stack_frame instruction;
            aux instructions
        | closures_instructions, _ ->
            compile_closure_allocation cir context stack_frame
              closures_instructions;
            aux instructions
        end
      end
  in

  aux body

and compile_closure_allocation cir context stack_frame closure_instructions =
  List.iter
    begin function
      | Code.Let (variable, Code.Closure (params, (label_address, _))), _ ->
          let closure_info =
            Hashtbl.find context.Context.closures label_address
          in

          let closure =
            Cir.Closure
              {
                name = Printf.sprintf "c%d" label_address;
                arity = List.length params;
                free_variables_count = List.length closure_info.free_variables;
              }
          in

          let slot = Stack_frame.find_variable_slot stack_frame variable in

          Cir_program.add cir @@ Set_stack_frame_variable (slot, closure)
      | _ -> ()
    end
    closure_instructions;

  List.iter
    begin function
      | Code.Let (variable, Code.Closure (_, (label_address, _))), _ ->
          let closure_info =
            Hashtbl.find context.Context.closures label_address
          in

          List.iter
            (fun free_variable ->
              let expression =
                match
                  Stack_frame.find_variable_slot_opt stack_frame free_variable
                with
                | Some slot -> Cir.Get_stack_frame_variable slot
                | None -> Cir.Get_variable free_variable
              in

              let slot = Stack_frame.find_variable_slot stack_frame variable in
              Cir_program.add cir
              @@ Add_closure_argument (Get_stack_frame_variable slot, expression))
            closure_info.free_variables
      | _ -> ()
    end
    closure_instructions

and compile_branch cir context stack_frame already_visited branch =
  match branch with
  | Code.Return variable ->
      let expression =
        match Stack_frame.find_variable_slot_opt stack_frame variable with
        | Some slot -> Cir.Get_stack_frame_variable slot
        | None -> Cir.Get_variable variable
      in

      Cir_program.add cir @@ Cir.Return expression
  | Code.Raise (variable, _) ->
      let expression =
        match Stack_frame.find_variable_slot_opt stack_frame variable with
        | Some slot -> Cir.Get_stack_frame_variable slot
        | None -> Cir.Get_variable variable
      in

      Cir_program.add cir @@ Cir.Raise expression
  | Code.Stop ->
      Cir_program.add cir @@ Cir.Return (Constanta (Raw_c " Val_unit"))
  | Code.Branch (target_label_address, arguments) ->
      compile_environment_assigns cir context stack_frame target_label_address
        arguments;

      Cir_program.add cir @@ Cir.Goto target_label_address;
      compile_block cir context stack_frame already_visited target_label_address
  | Cond
      ( cond_variable,
        (then_label_address, then_branch_arguments),
        (else_label_address, else_branch_arguments) ) ->
      let cond_expression =
        match Stack_frame.find_variable_slot_opt stack_frame cond_variable with
        | Some slot -> Cir.Get_stack_frame_variable slot
        | None -> Cir.Get_variable cond_variable
      in

      let cir_then_arguments = Cir_program.create () in
      compile_environment_assigns cir_then_arguments context stack_frame
        then_label_address then_branch_arguments;

      let cir_else_arguments = Cir_program.create () in
      compile_environment_assigns cir_else_arguments context stack_frame
        else_label_address else_branch_arguments;

      Cir_program.add cir
      @@ Condition
           ( Type_val (`Bool, cond_expression),
             Cir_program.to_list cir_then_arguments
             @ [ Goto then_label_address ],
             Cir_program.to_list cir_else_arguments
             @ [ Goto else_label_address ] );

      compile_block cir context stack_frame already_visited then_label_address;
      compile_block cir context stack_frame already_visited else_label_address
  | Code.Switch _ -> failwith "switch"
  | Code.Pushtrap _ -> failwith "pushtrap"
  | Code.Poptrap _ -> failwith "poptrap"

and compile_instruction cir context stack_frame (instruction, _) =
  match instruction with
  | Code.Let (variable, expression) ->
      let expression = compile_expression context stack_frame expression in

      let instruction =
        match Stack_frame.find_variable_slot_opt stack_frame variable with
        | Some slot -> Cir.Set_stack_frame_variable (slot, expression)
        | None -> Cir.Set_variable (variable, expression)
      in

      Cir_program.add cir instruction
  | Code.Assign _ -> failwith ""
  | Code.Set_field _ -> failwith ""
  | Code.Offset_ref _ -> failwith ""
  | Code.Array_set _ -> failwith ""

and compile_constanta context constanta =
  match constanta with
  | Code.Int x -> Cir.Int (Int32.to_int x)
  | String s | NativeString (Byte s | Utf (Utf8 s)) ->
      String_interner.insert context.Context.string_interner s;
      Cir.Raw_c (Printf.sprintf "s_%d" @@ String.hash s)
  | Float float -> Cir.Raw_c Printf.(sprintf "caml_copy_double(%h)" float)
  | Int64 x -> Cir.Raw_c Printf.(sprintf "caml_copy_int64(%LdLL)" x)
  | Tuple (tag, constants, _) ->
      Cir.Tuple
        {
          tag;
          constants =
            Array.to_list constants |> List.map (compile_constanta context);
        }
  | Float_array _ -> Cir.Raw_c "UNSUPPORTED_FLOAT_ARRAY"

and compile_expression context stack_frame expression =
  match expression with
  | Code.Constant constanta -> Constanta (compile_constanta context constanta)
  | Code.Field (variable, index) -> Cir.Field (variable, index)
  | Code.Block (tag, fields, _, _) ->
      let fields =
        Array.to_list fields
        |> List.map @@ fun variable ->
           match Stack_frame.find_variable_slot_opt stack_frame variable with
           | Some slot -> Cir.Get_stack_frame_variable slot
           | None -> Cir.Get_variable variable
      in
      Cir.Block { tag; fields }
  | Code.Closure _ -> assert false
  | Code.Apply { f; args; _ } ->
      let f =
        match Stack_frame.find_variable_slot_opt stack_frame f with
        | Some slot -> Cir.Get_stack_frame_variable slot
        | None -> Cir.Get_variable f
      in

      let args =
        List.map
          (fun arg ->
            match Stack_frame.find_variable_slot_opt stack_frame arg with
            | Some slot -> Cir.Get_stack_frame_variable slot
            | None -> Cir.Get_variable arg)
          args
      in

      Cir.Call { f; args }
  | Special Undefined -> Constanta (Cir.Raw_c "Val_unit")
  | Special (Alias_prim _) -> Constanta (Cir.Raw_c "Val_unit")
  | Prim (prim, args) -> compile_prim context stack_frame prim args

and compile_prim context stack_frame prim args =
  let compile_arg = function
    | Code.Pv variable ->
        begin match Stack_frame.find_variable_slot_opt stack_frame variable with
        | Some slot -> Cir.Get_stack_frame_variable slot
        | None -> Cir.Get_variable variable
        end
    | Pc c -> Cir.Constanta (compile_constanta context c)
  in

  match (prim, args) with
  | Code.Vectlength, [ x ] -> Cir.Type_val (`Int, compile_arg x)
  | Array_get, [ array_variable; index ] ->
      let array_variable = compile_arg array_variable in
      let array_index = compile_arg index in

      Cir.Field' (array_variable, array_index)
  | Code.Extern "%undefined", _ -> Cir.Constanta (Raw_c "Val_unit")
  | Code.Extern name, [ first_operand; second_operand ]
    when String.starts_with ~prefix:"%" name
         && List.mem_assoc name integer_binary_operations ->
      compile_integer_binary_operations name
        (compile_arg first_operand)
        (compile_arg second_operand)
  | Code.Extern name, args when String.starts_with ~prefix:"%" name ->
      failwith
      @@ Format.sprintf "EXTERN %s with args %d" name (List.length args)
  | Code.Extern name, _ ->
      Cir.Call_extern
        { function_name = name; arguments = List.map compile_arg args }
  | Not, [ arg ] -> Cir.Val_type (`Bool, Not (Type_val (`Bool, compile_arg arg)))
  | IsInt, [ arg ] -> Cir.Val_type (`Bool, Is_int (compile_arg arg))
  | Eq, [ first_operand; second_operand ] ->
      Cir.Val_type
        ( `Bool,
          Equal
            ( Type_val (`Int, compile_arg first_operand),
              Type_val (`Int, compile_arg second_operand) ) )
  | Neq, [ first_operand; second_operand ] ->
      Cir.Val_type
        ( `Bool,
          Not (Equal (compile_arg first_operand, compile_arg second_operand)) )
  | Lt, [ first_operand; second_operand ] ->
      Cir.Val_type
        ( `Bool,
          Less_than (compile_arg first_operand, compile_arg second_operand) )
  | Le, [ first_operand; second_operand ] ->
      Cir.Val_type
        ( `Bool,
          Less_than_or_equal
            (compile_arg first_operand, compile_arg second_operand) )
  | _ -> failwith "unhandled"

and compile_integer_binary_operations operation first_operand second_operand =
  let integer_operation =
    List.assoc ~eq:String.equal operation integer_binary_operations
  in

  let first = Cir.Type_val (`Int, first_operand)
  and second = Cir.Type_val (`Int, second_operand) in

  Cir.Val_type (`Int, Binary_operation (integer_operation, first, second))

let compile_program program =
  let context = Context.make program in

  let cir_program = Cir_program.create () in

  Hashtbl.iter
    (fun label_address closure_info ->
      compile_closure cir_program context label_address closure_info)
    context.closures;

  (context, cir_program, context.string_interner)
