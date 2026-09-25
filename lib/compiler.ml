open Containers
module Code = Js_of_ocaml_compiler.Code

(** Metadata extracted for code closures during static analysis. *)
module Closure = struct
  type info = {
    free_variables : Code.Var.t list;
        (** Free variables captured by the closure. *)
    continues : Code.cont;  (** Target continuation information. *)
    parameters : Code.Var.t list;  (** Parameters accepted by the closure. *)
  }
end

module String_interner = CCHashSet.Make (String)
(** Hash set used for interning string constants. *)

(** Compilation context. *)
module Context = struct
  type t = {
    program : Code.program;
    closures : closures;
    string_interner : String_interner.t;
  }
  (** Global compilation context state. *)

  and closures = (Code.Addr.t, Closure.info) Hashtbl.t
  (** Mapping from block entry address to closure metadata. *)

  (** Extracts closure definitions from the IR program. *)
  let extract_closures program : closures =
    let free_vars = Js_of_ocaml_compiler.Freevars.f program in

    let extract_closure parameters ((label_address, _) as continues) =
      let free_variables =
        Code.Addr.Map.find_opt label_address free_vars
        |> Option.map_or ~default:[] Code.Var.Set.elements
      in

      (label_address, Closure.{ free_variables; parameters; continues })
    in

    Code.fold_closures program
      (fun _ parameters continues _ acc ->
        extract_closure parameters continues :: acc)
      []
    |> Hashtbl.of_list

  (** Initializes a new compilation context for the given [program]. *)
  let make program =
    let closures = extract_closures program in
    let string_interner = String_interner.create 100 in
    { program; closures; string_interner }

  (** Collects all variables defined or used within the block hierarchy starting
      at [label_address]. *)
  let extract_variables_at_block context label_address =
    let variables = Dynarray.create () in

    let fold = Code.fold_children in
    Code.traverse { fold }
      begin fun label_address () ->
        let block = Code.Addr.Map.find label_address context.program.blocks in

        Dynarray.append_list variables block.params;
        Dynarray.append_list variables
        @@ List.filter_map
             (function Code.Let (variable, _) -> Some variable | _ -> None)
             block.Code.body;

        match block.branch with
        | Pushtrap (_, exn_var, _) -> Dynarray.add_last variables exn_var
        | _ -> ()
      end
      label_address context.program.blocks ();

    Dynarray.to_list variables
end

(** Manages stack frame slot allocations for local variables. *)
module Stack_frame = struct
  type t = {
    variables : (Code.Var.t * slot, Vector.rw) Vector.t;
    mutable current_slot : int;
  }

  and slot = int
  (** Identifier for a stack slot index. *)

  (** Creates an empty stack frame. *)
  let create () = { variables = Vector.create (); current_slot = 0 }

  (** Returns the stack slot allocated for [variable], or [None] if not present.
  *)
  let find_variable_slot_opt { variables; _ } variable =
    Vector.find_map
      (fun (v, slot) -> if Code.Var.equal v variable then Some slot else None)
      variables

  (** Returns the stack slot allocated for [variable], raising an exception if
      not found. *)
  let find_variable_slot frame variable =
    find_variable_slot_opt frame variable
    |> Option.get_exn_or "not found variable at stack frame"

  (** Allocates a new stack slot for [variable] in [frame]. *)
  let push frame variable =
    let slot = frame.current_slot in
    frame.current_slot <- succ frame.current_slot;

    Vector.push frame.variables (variable, slot)

  (** Allocates slots for a list of [variables] in [frame]. *)
  let pushes frame variables = List.iter (push frame) variables

  (** Returns the total number of variable slots in [frame]. *)
  let size { variables; _ } = Vector.length variables

  (** Returns [true] if [variable] is not allocated in [frame]. *)
  let not_in { variables; _ } variable =
    not @@ Vector.exists (fun (v, _) -> Code.Var.equal variable v) variables
end

(** Builder buffer for generating CIR (C Intermediate Representation)
    instructions. *)
module Cir_program = struct
  type t = Cir.instruction Dynarray.t

  let create : unit -> t = Dynarray.create
  let add : t -> Cir.instruction -> unit = Dynarray.add_last
  let to_list : t -> _ list = Dynarray.to_list
  let pp ppf instars = Dynarray.iter (Cir.pp_instruction ppf) instars
end

(** Mapping from primitive integer operation names to C operators. *)
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

(** Compiles a closure into a standalone CIR function definition. *)
let rec compile_closure cir context label_address closure_info =
  let stack_frame = Stack_frame.create () in

  Context.extract_variables_at_block context label_address
  |> Stack_frame.pushes stack_frame;

  let closure_variables =
    closure_info.Closure.free_variables @ closure_info.parameters
  in

  List.iter
    begin fun variable ->
      if Stack_frame.not_in stack_frame variable then
        Stack_frame.push stack_frame variable
    end
    closure_variables;

  let required_stack_size = Stack_frame.size stack_frame in
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

    emits_assignments_of_passing_arguments cir_closure context stack_frame
      label_address
      (snd closure_info.continues);

    compile_block cir_closure context stack_frame
      Dynarray.(create ())
      label_address
  end;

  Cir_program.add cir
    Cir.(
      Closure_definition
        (Printf.sprintf "c%d" label_address, Cir_program.to_list cir_closure))

(** Emits assignments passing arguments to block parameters via temporary
    variables. *)
and emits_assignments_of_passing_arguments =
  let uniq_id = ref 0 in

  fun cir context stack_frame label_address arguments ->
    let id = !uniq_id in
    incr uniq_id;

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

(** Compiles a basic block at [label_address], guarding against cyclic
    traversals. *)
and compile_block cir context stack_frame already_visited label_address =
  if not @@ Dynarray.exists (( = ) label_address) already_visited then begin
    Dynarray.add_last already_visited label_address;

    Cir_program.add cir (Label label_address);

    let block =
      Code.Addr.Map.find label_address context.Context.program.blocks
    in

    compile_block_body cir context stack_frame block.Code.body;
    compile_branch cir context stack_frame already_visited block.Code.branch
  end

(** Compiles the sequence of instructions in a basic block body. *)
and compile_block_body cir context stack_frame body =
  let take_closures instructions =
    List.take_while
      (function Code.Let (_, Code.Closure _) -> true | _ -> false)
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

(** Emits CIR code to instantiate closures and capture their free variables. *)
and compile_closure_allocation cir context stack_frame closure_instructions =
  List.iter
    begin function
      | Code.Let (variable, Code.Closure (params, (label_address, _), _)) ->
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
      | Code.Let (variable, Code.Closure (_, (label_address, _), _)) ->
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

(** Compiles block control-flow operations (return, jump, branch, raise). *)
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
      emits_assignments_of_passing_arguments cir context stack_frame
        target_label_address arguments;

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
      emits_assignments_of_passing_arguments cir_then_arguments context
        stack_frame then_label_address then_branch_arguments;

      let cir_else_arguments = Cir_program.create () in
      emits_assignments_of_passing_arguments cir_else_arguments context
        stack_frame else_label_address else_branch_arguments;

      Cir_program.add cir
      @@ Condition
           ( Type_val (`Bool, cond_expression),
             Cir_program.to_list cir_then_arguments
             @ [ Goto then_label_address ],
             Cir_program.to_list cir_else_arguments
             @ [ Goto else_label_address ] );

      compile_block cir context stack_frame already_visited then_label_address;
      compile_block cir context stack_frame already_visited else_label_address
  | Code.Switch (switch_variable, cases) ->
      let cases_branches =
        Array.to_list cases
        |> List.mapi @@ fun i (label_address, arguments) ->
           let cir = Cir_program.create () in

           emits_assignments_of_passing_arguments cir context stack_frame
             label_address arguments;
           Cir_program.add cir @@ Goto label_address;

           compile_block cir context stack_frame already_visited label_address;

           (i, Cir_program.to_list cir)
      in

      let slot = Stack_frame.find_variable_slot stack_frame switch_variable in

      Cir_program.add cir
      @@ Cir.Switch (Get_stack_frame_variable slot, cases_branches)
  | Pushtrap
      ( (body_label_address, body_arguments),
        exception_variable,
        (handler_label_address, handler_arguments) ) ->
      let cir_body = Cir_program.create () in
      emits_assignments_of_passing_arguments cir_body context stack_frame
        body_label_address body_arguments;
      Cir_program.add cir_body (Goto body_label_address);

      let cir_handler = Cir_program.create () in
      Cir_program.add cir_handler
      @@ compile_set_local_variable stack_frame exception_variable
           Cir.(Constanta (Raw_c {|exn_value|}));
      emits_assignments_of_passing_arguments cir_handler context stack_frame
        handler_label_address handler_arguments;
      Cir_program.add cir_handler (Goto handler_label_address);

      Cir_program.add cir
      @@ Cir.Push_trap
           {
             body = Cir_program.to_list cir_body;
             handler = Cir_program.to_list cir_handler;
           };

      compile_block cir context stack_frame already_visited body_label_address;
      compile_block cir context stack_frame already_visited
        handler_label_address
  | Poptrap (label_address, arguments) ->
      Cir_program.add cir Pop_trap;
      emits_assignments_of_passing_arguments cir context stack_frame
        label_address arguments;
      compile_block cir context stack_frame already_visited label_address

(** Compiles a single IR instruction into CIR. *)
and compile_instruction cir context stack_frame instruction =
  let compile_access_to_local_variable =
    compile_get_local_variable stack_frame
  in

  match instruction with
  | Code.Let (variable, expression) ->
      let expression = compile_expression context stack_frame expression in

      let instruction =
        match Stack_frame.find_variable_slot_opt stack_frame variable with
        | Some slot -> Cir.Set_stack_frame_variable (slot, expression)
        | None -> Cir.Set_variable (variable, expression)
      in

      Cir_program.add cir instruction
  | Code.Assign _ -> failwith "assign"
  | Code.Offset_ref _ -> failwith "offset_ref"
  | Code.Set_field (block, index, _, x) ->
      Cir_program.add cir
      @@ Cir.Set_field
           ( compile_access_to_local_variable block,
             Constanta (Raw_c (string_of_int index)),
             compile_access_to_local_variable x )
  | Code.Array_set (array, index, x) ->
      Cir_program.add cir
      @@ Cir.Set_field
           ( compile_access_to_local_variable array,
             Type_val (`Int, compile_access_to_local_variable index),
             compile_access_to_local_variable x )
  | Code.Event _ -> failwith "unsupported event"

(** Translates a literal constant into its CIR equivalent. *)
and compile_constant context constanta =
  match (constanta : Code.constant) with
  | NativeInt x | Int32 x -> Cir.Int (Int32.to_int x)
  | Int target_int ->
      Cir.Int (Js_of_ocaml_compiler.Targetint.to_int_exn target_int)
  | String s | NativeString (Byte s | Utf (Utf8 s)) ->
      String_interner.insert context.Context.string_interner s;
      Cir.String s
  | Float32 _ | Float _ -> Cir.Float 0.1
  | Null_ -> failwith ""
  | Int64 x -> Cir.Int64 x
  | Tuple (tag, constants, _) ->
      Cir.Tuple
        {
          tag;
          constants =
            Array.to_list constants |> List.map (compile_constant context);
        }
  | Float_array _ -> Cir.Raw_c "UNSUPPORTED_FLOAT_ARRAY"

(** Translates an IR expression into CIR. *)
and compile_expression context stack_frame expression =
  match expression with
  | Code.Constant constanta -> Constanta (compile_constant context constanta)
  | Code.Field (variable, index, _) ->
      begin match Stack_frame.find_variable_slot_opt stack_frame variable with
      | None -> Cir.Field (variable, index)
      | Some slot ->
          Cir.Field'
            ( Get_stack_frame_variable slot,
              Constanta (Raw_c (string_of_int index)) )
      end
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
  | Special (Alias_prim _) -> Constanta (Cir.Raw_c "Val_unit")
  | Prim (prim, args) -> compile_primitive context stack_frame prim args

(** Compiles a primitive to CIR. *)
and compile_primitive context stack_frame prim args =
  let compile_argument = compile_argument context stack_frame in

  match (prim, args) with
  | Code.Vectlength, [ x ] -> Cir.Type_val (`Int, compile_argument x)
  | Array_get, [ array_variable; index ] ->
      let array_variable = compile_argument array_variable in
      let array_index = Cir.Type_val (`Int, compile_argument index) in

      Cir.Field' (array_variable, array_index)
  | Code.Extern "%undefined", _ -> Cir.Constanta (Raw_c "Val_unit")
  | Code.Extern name, [ first_operand; second_operand ]
    when String.starts_with ~prefix:"%" name
         && List.mem_assoc name integer_binary_operations ->
      compile_integer_binary_operations name
        (compile_argument first_operand)
        (compile_argument second_operand)
  | Code.Extern name, _ when String.starts_with ~prefix:"%" name ->
      compile_extern_builtins context stack_frame name args
  | Code.Extern name, _ -> compile_extern context stack_frame name args
  | Not, [ arg ] ->
      Cir.Val_type (`Bool, Not (Type_val (`Bool, compile_argument arg)))
  | IsInt, [ arg ] -> Cir.Val_type (`Bool, Is_int (compile_argument arg))
  | Eq, [ first_operand; second_operand ] ->
      Cir.Val_type
        ( `Bool,
          Equal
            ( Type_val (`Int, compile_argument first_operand),
              Type_val (`Int, compile_argument second_operand) ) )
  | Neq, [ first_operand; second_operand ] ->
      Cir.Val_type
        ( `Bool,
          Not
            (Equal
               (compile_argument first_operand, compile_argument second_operand))
        )
  | Lt, [ first_operand; second_operand ] ->
      Cir.Val_type
        ( `Bool,
          Less_than
            (compile_argument first_operand, compile_argument second_operand) )
  | Le, [ first_operand; second_operand ] ->
      Cir.Val_type
        ( `Bool,
          Less_than_or_equal
            (compile_argument first_operand, compile_argument second_operand) )
  | _ -> Cir.Constanta (Raw_c "Val_unit")

(** Translates binary integer arithmetic and bitwise primitives to CIR binary
    operations. *)
and compile_integer_binary_operations operation first_operand second_operand =
  let integer_operation =
    List.assoc ~eq:String.equal operation integer_binary_operations
  in

  let first = Cir.Type_val (`Int, first_operand)
  and second = Cir.Type_val (`Int, second_operand) in

  Cir.Val_type (`Int, Binary_operation (integer_operation, first, second))

(** Compiles built-in compiler primitives prefixed with [%] into equivalent CIR
    expressions. *)
and compile_extern_builtins context stack_frame name arguments =
  match (name, arguments) with
  | "%caml_format_int_special", [ arg ] ->
      Cir.Call_extern
        ( `Name "caml_format_int",
          [ Constanta (Raw_c "%%d"); compile_argument context stack_frame arg ]
        )
  | "%direct_obj_tag", [ arg ] ->
      Cir.Val_type
        (`Int, Get_block (`Tag, compile_argument context stack_frame arg))
  | "%int_neg", [ arg ] ->
      Cir.Val_type
        ( `Int,
          Negative (Type_val (`Int, compile_argument context stack_frame arg))
        )
  | "%int_lsr", [ first_operand; second_operand ] ->
      Cir.Val_type
        ( `Int,
          Binary_operation
            ( ">>",
              Type_val (`Int, compile_argument context stack_frame first_operand),
              Type_val
                (`Int, compile_argument context stack_frame second_operand) ) )
  | _ -> failwith name

(** Compiles external function calls or C runtime primitives into CIR
    expressions. *)
and compile_extern context stack_frame name arguments =
  match (name, arguments) with
  | "caml_array_unsafe_get", [ array; index ] ->
      Cir.Field'
        ( compile_argument context stack_frame array,
          Type_val (`Int, compile_argument context stack_frame index) )
  | _ ->
      Cir.Call_extern
        (`Name name, List.map (compile_argument context stack_frame) arguments)

(** Compiles a primitive operand argument (variable or literal constant) into a
    CIR expression. *)
and compile_argument (context : Context.t) (stack_frame : Stack_frame.t)
    argument =
  match argument with
  | Code.Pv variable ->
      begin match Stack_frame.find_variable_slot_opt stack_frame variable with
      | Some slot -> Cir.Get_stack_frame_variable slot
      | None -> Cir.Get_variable variable
      end
  | Pc constant -> Cir.Constanta (compile_constant context constant)

and compile_get_local_variable stack_frame variable =
  match Stack_frame.find_variable_slot_opt stack_frame variable with
  | Some slot -> Cir.Get_stack_frame_variable slot
  | None -> Cir.Get_variable variable

and compile_set_local_variable stack_frame variable expression =
  match Stack_frame.find_variable_slot_opt stack_frame variable with
  | None -> Cir.Set_variable (variable, expression)
  | Some slot -> Cir.Set_stack_frame_variable (slot, expression)

(** Compiles an entire [Code.program] into a compilation context, CIR program,
    and string interner. *)
let compile_program program =
  let context = Context.make program in

  let cir_program = Cir_program.create () in

  Hashtbl.iter
    (fun label_address closure_info ->
      compile_closure cir_program context label_address closure_info)
    context.closures;

  (context, cir_program, context.string_interner)
