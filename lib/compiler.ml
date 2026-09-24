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
  type t = (Code.Var.t * slot, Vector.rw) Vector.t
  and slot = int

  let create : unit -> t = Vector.create

  let find_variable_slot_opt frame variable =
    Vector.find_map
      (fun (v, slot) -> if Code.Var.equal v variable then Some slot else None)
      frame

  let find_variable_slot frame variable =
    find_variable_slot_opt frame variable
    |> Option.get_exn_or "not found variable at stack frame"

  let get frame variable =
    match find_variable_slot_opt frame variable with
    | Some slot -> `Slot slot
    | None -> `Name "v_varname"

  let get_last_slot frame =
    Vector.top frame |> Option.map_or ~default:0 (fun (_, slot) -> slot)

  let add_variable frame variable =
    let slot = succ @@ get_last_slot frame in
    Vector.push frame (variable, slot)

  let add_variables frame variables = List.iter (add_variable frame) variables
  let count_variables frame = Vector.length frame

  let variable_not_exist frame variable =
    not @@ Vector.exists (fun (v, _) -> Code.Var.equal variable v) frame
end

module Cir_program = struct
  type t = Cir.instruction Dynarray.t

  let create : unit -> t = Dynarray.create
  let add : t -> Cir.instruction -> unit = Dynarray.add_last
  let to_list : t -> _ list = Dynarray.to_list
  let pp ppf instars = Dynarray.iter (Cir.pp_instruction ppf) instars
end

let rec compile_closure cir context label_address closure_info =
  let stack_frame = Stack_frame.create () in

  Context.find_variables_at_block context label_address
  |> Stack_frame.add_variables stack_frame;

  List.iter
    begin fun variable ->
      if Stack_frame.variable_not_exist stack_frame variable then
        Stack_frame.add_variable stack_frame variable
    end
    (closure_info.Closure.free_variables @ closure_info.parameters);

  let required_stack_size = Stack_frame.count_variables stack_frame in

  let cir_closure = Cir_program.create () in
  begin
    Cir_program.add cir_closure (Cir.Reserve_stack_size required_stack_size);

    List.iteri
      (fun env_index variable ->
        let slot = Stack_frame.find_variable_slot stack_frame variable in
        let env = Printf.sprintf "env[%d]" env_index in

        Cir_program.add cir_closure
          Cir.(Set_stack_frame_variable (variable, slot, Constanta (Raw_c env))))
      (closure_info.Closure.free_variables @ closure_info.parameters);

    resolve_temporal_variables_at_block cir_closure context stack_frame
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

and resolve_temporal_variables_at_block cir context stack_frame label_address
    variables =
  let temporal_variables =
    (Code.Addr.Map.find label_address context.Context.program.blocks).params
  in

  List.iteri
    (fun _ temporal_variable ->
      let definition =
        Cir.Variable_definition
          ( "tLOX",
            Cir.Get_stack_frame_variable
              (Stack_frame.find_variable_slot stack_frame temporal_variable) )
      in
      Cir_program.add cir definition)
    variables;

  List.iteri
    (fun _ temporal_variable ->
      Cir_program.add cir
        Cir.(
          Set_stack_frame_variable
            ( temporal_variable,
              Stack_frame.find_variable_slot stack_frame temporal_variable,
              Constanta (Raw_c "tLOX2") )))
    temporal_variables

and compile_block cir context stack_frame already_visited label_address =
  if not @@ Dynarray.exists (( = ) label_address) already_visited then begin
    Dynarray.add_last already_visited label_address;

    let block =
      Code.Addr.Map.find label_address context.Context.program.blocks
    in
    List.iter (compile_instruction cir context stack_frame) block.Code.body
  end

and compile_instruction cir context stack_frame (instruction, _) =
  match instruction with
  | Code.Let (variable, expression) ->
      Cir_program.add cir
        Cir.(
          Set_stack_frame_variable
            ( variable,
              Stack_frame.find_variable_slot stack_frame variable,
              compile_expression context stack_frame expression ))
  | Code.Assign _ -> failwith ""
  | Code.Set_field _ -> failwith ""
  | Code.Offset_ref _ -> failwith ""
  | Code.Array_set _ -> failwith ""

and compile_constanta context constanta =
  match constanta with
  | Code.Int x -> Cir.Raw_c Printf.(sprintf "Val_int(%ldL)" x)
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
      Cir.Block { tag; fields = Array.to_list fields }
  | Code.Closure _ -> assert false
  | Code.Apply { f; args; _ } -> Cir.Call { f; args }
  | Special Undefined -> Constanta (Cir.Raw_c "Val_unit")
  | Special (Alias_prim _) -> Constanta (Cir.Raw_c "Val_unit")
  | Prim (prim, args) -> compile_prim context stack_frame prim args

and compile_prim context stack_frame prim args =
  let compile_arg = function
    | Code.Pv variable ->
        Cir.Get_stack_frame_variable
          (Stack_frame.find_variable_slot stack_frame variable)
    | Pc c -> Cir.Constanta (compile_constanta context c)
  in

  match (prim, args) with
  | Code.Vectlength, [ x ] -> Cir.To_int (compile_arg x)
  | Extern "%undefined", _ -> Cir.Constanta (Raw_c "Val_unit")
  | Extern name, _ ->
      Cir.Call_extern
        { function_name = name; arguments = List.map compile_arg args }
  | _ -> Cir.Constanta (Raw_c "LOX")

let compile_program program =
  let context = Context.make program in

  let cir_program = Cir_program.create () in

  Hashtbl.iter
    (fun label_address closure_info ->
      compile_closure cir_program context label_address closure_info)
    context.closures;

  (cir_program, context.string_interner)
