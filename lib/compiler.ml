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

  (** Returns the stack slot allocated for [variable], or [None] if absent. *)
  let find_variable_slot_opt { variables; _ } variable =
    Vector.find_map
      (fun (v, slot) -> if Code.Var.equal v variable then Some slot else None)
      variables

  (** Returns the stack slot allocated for [variable], raising if absent. *)
  let find_variable_slot frame variable =
    find_variable_slot_opt frame variable
    |> Option.get_exn_or "variable not found in stack frame"

  (** Allocates a new stack slot for [variable] in [frame]. *)
  let push frame variable =
    let slot = frame.current_slot in
    frame.current_slot <- slot + 1;
    Vector.push frame.variables (variable, slot)

  (** Allocates slots for a list of [variables] in [frame]. *)
  let pushes frame variables = List.iter (push frame) variables

  (** Returns the total number of allocated slots in [frame]. *)
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
  let pp ppf instructions = Dynarray.iter (Cir.pp_instruction ppf) instructions
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

(** Reads local [variable] from the stack frame if it is allocated there,
    otherwise falls back to a global variable lookup. *)
let compile_get_local_variable stack_frame variable =
  match Stack_frame.find_variable_slot_opt stack_frame variable with
  | Some slot -> Cir.Get_stack_frame_variable { slot }
  | None -> Cir.Get_variable variable

(** Binds [var] to [value], targeting the stack frame slot when available. *)
let compile_set_local_variable stack_frame var value =
  match Stack_frame.find_variable_slot_opt stack_frame var with
  | None -> Cir.Set_variable { var; value }
  | Some slot -> Cir.Set_stack_frame_variable { slot; value }

(** Emits assignments passing [arguments] to the parameters of the block at
    [label_address]. Arguments are first copied into temporaries so that the
    parallel assignment does not clobber sources that are also destinations. *)
let emit_argument_passing =
  let next_id = ref 0 in

  fun cir context stack_frame label_address arguments ->
    let id = !next_id in
    incr next_id;

    let temp_name i = Printf.sprintf "t%d_i%d" id i in

    let params =
      (Code.Addr.Map.find label_address context.Context.program.blocks).params
    in

    List.iteri
      begin fun i argument ->
        let name = temp_name i in
        Cir_program.add cir @@ Variable_declaration { name };
        Cir_program.add cir
        @@ Variable_definition
             { name; value = compile_get_local_variable stack_frame argument }
      end
      arguments;

    List.iteri
      begin fun i param ->
        let value = Cir.Raw_c (temp_name i) in
        Cir_program.add cir
        @@ compile_set_local_variable stack_frame param value
      end
      params

(** Compiles a closure into a standalone CIR function definition. *)
let rec compile_closure cir context label_address info =
  let stack_frame = Stack_frame.create () in

  Context.extract_variables_at_block context label_address
  |> Stack_frame.pushes stack_frame;

  let env_variables = info.Closure.free_variables @ info.parameters in

  List.iter
    begin fun variable ->
      if Stack_frame.not_in stack_frame variable then
        Stack_frame.push stack_frame variable
    end
    env_variables;

  let stack_size = Stack_frame.size stack_frame in
  let body = Cir_program.create () in
  begin
    Cir_program.add body (Cir.Reserve_stack_size stack_size);

    List.iteri
      begin fun env_index variable ->
        let slot = Stack_frame.find_variable_slot stack_frame variable in

        Cir_program.add body
        @@ Set_stack_frame_variable
             { slot; value = Raw_c (Printf.sprintf "env[%d]" env_index) }
      end
      env_variables;

    emit_argument_passing body context stack_frame label_address
      (snd info.continues);

    compile_block body context stack_frame (Dynarray.create ()) label_address
  end;

  Cir_program.add cir
  @@ Closure_definition
       {
         name = Printf.sprintf "c%d" label_address;
         branch = Cir_program.to_list body;
       }

(** Compiles the basic block at [label_address], guarding against cyclic
    traversals. *)
and compile_block cir context stack_frame visited label_address =
  if not @@ Dynarray.exists (( = ) label_address) visited then begin
    Dynarray.add_last visited label_address;

    Cir_program.add cir (Label label_address);

    let block =
      Code.Addr.Map.find label_address context.Context.program.blocks
    in

    compile_block_body cir context stack_frame block.Code.body;
    compile_branch cir context stack_frame visited block.Code.branch
  end

(** Compiles the sequence of instructions in a basic block body. *)
and compile_block_body cir context stack_frame body =
  let is_closure_let = function
    | Code.Let (_, Code.Closure _) -> true
    | _ -> false
  in

  let rec loop = function
    | [] -> ()
    | instructions -> begin
        let leading_closures = List.take_while is_closure_let instructions in
        let rest = List.drop (List.length leading_closures) instructions in

        match (leading_closures, rest) with
        | [], [] -> ()
        | [], instruction :: rest ->
            compile_instruction cir context stack_frame instruction;
            loop rest
        | leading_closures, _ ->
            compile_closure_allocation cir context stack_frame leading_closures;
            loop rest
      end
  in

  loop body

(** Emits CIR code to instantiate closures and capture their free variables. *)
and compile_closure_allocation cir context stack_frame closure_instructions =
  List.iter
    begin function
      | Code.Let (variable, Code.Closure (params, (label_address, _), _)) ->
          let info = Hashtbl.find context.Context.closures label_address in

          let value =
            Cir.Closure
              {
                name = Printf.sprintf "c%d" label_address;
                arity = List.length params;
                fvc = List.length info.free_variables;
              }
          in

          let slot = Stack_frame.find_variable_slot stack_frame variable in
          Cir_program.add cir @@ Set_stack_frame_variable { slot; value }
      | _ -> ()
    end
    closure_instructions;

  List.iter
    begin function
      | Code.Let (variable, Code.Closure (_, (label_address, _), _)) ->
          let info = Hashtbl.find context.Context.closures label_address in
          let slot = Stack_frame.find_variable_slot stack_frame variable in

          List.iter
            begin fun free_variable ->
              let arg = compile_get_local_variable stack_frame free_variable in

              Cir_program.add cir
              @@ Add_closure_argument
                   { var = Get_stack_frame_variable { slot }; arg }
            end
            info.free_variables
      | _ -> ()
    end
    closure_instructions

(** Compiles block control-flow operations (return, jump, branch, raise). *)
and compile_branch cir context stack_frame visited branch =
  match branch with
  | Code.Return variable ->
      Cir_program.add cir
      @@ Cir.Return (compile_get_local_variable stack_frame variable)
  | Code.Raise (variable, _) ->
      Cir_program.add cir
      @@ Cir.Raise (compile_get_local_variable stack_frame variable)
  | Code.Stop -> Cir_program.add cir @@ Cir.Return (Raw_c " Val_unit")
  | Code.Branch (target, arguments) ->
      emit_argument_passing cir context stack_frame target arguments;

      Cir_program.add cir @@ Cir.Goto target;
      compile_block cir context stack_frame visited target
  | Cond (cond, (then_label, then_args), (else_label, else_args)) ->
      let condition = compile_get_local_variable stack_frame cond in

      let then_branch = Cir_program.create () in
      emit_argument_passing then_branch context stack_frame then_label then_args;

      let else_branch = Cir_program.create () in
      emit_argument_passing else_branch context stack_frame else_label else_args;

      Cir_program.add cir
      @@ Condition
           {
             condition = Type_val (`Bool, condition);
             then_branch = Cir_program.to_list then_branch @ [ Goto then_label ];
             else_branch = Cir_program.to_list else_branch @ [ Goto else_label ];
           };

      compile_block cir context stack_frame visited then_label;
      compile_block cir context stack_frame visited else_label
  | Code.Switch (switch_variable, cases) ->
      let case_branches =
        Array.to_list cases
        |> List.mapi @@ fun i (label_address, arguments) ->
           let case = Cir_program.create () in

           emit_argument_passing case context stack_frame label_address
             arguments;
           Cir_program.add case @@ Goto label_address;

           compile_block case context stack_frame visited label_address;

           (i, Cir_program.to_list case)
      in

      let slot = Stack_frame.find_variable_slot stack_frame switch_variable in

      Cir_program.add cir
      @@ Cir.Switch
           { condition = Get_stack_frame_variable { slot }; case_branches }
  | Pushtrap
      ( (body_label, body_args),
        exception_variable,
        (handler_label, handler_args) ) ->
      let body = Cir_program.create () in
      emit_argument_passing body context stack_frame body_label body_args;
      Cir_program.add body (Goto body_label);

      let handler = Cir_program.create () in
      Cir_program.add handler
      @@ compile_set_local_variable stack_frame exception_variable
           Cir.(Raw_c {|exn_value|});
      emit_argument_passing handler context stack_frame handler_label
        handler_args;
      Cir_program.add handler (Goto handler_label);

      Cir_program.add cir
      @@ Cir.Push_trap
           {
             branch = Cir_program.to_list body;
             handler_branch = Cir_program.to_list handler;
           };

      compile_block cir context stack_frame visited body_label;
      compile_block cir context stack_frame visited handler_label
  | Poptrap (label_address, arguments) ->
      Cir_program.add cir Pop_trap;
      emit_argument_passing cir context stack_frame label_address arguments;
      compile_block cir context stack_frame visited label_address

(** Compiles a single IR instruction into CIR. *)
and compile_instruction cir context stack_frame instruction =
  match instruction with
  | Code.Let (variable, expression) ->
      Cir_program.add cir
      @@ compile_set_local_variable stack_frame variable
      @@ compile_expression context stack_frame expression
  | Code.Assign (var, var2) ->
      Cir_program.add cir
      @@ Set_variable
           { var; value = compile_get_local_variable stack_frame var2 }
  | Code.Offset_ref (var, n) ->
      Cir_program.add cir
      @@ Offset_ref { var = compile_get_local_variable stack_frame var; n }
  | Code.Set_field (var, index, _, value) ->
      Cir_program.add cir
      @@ Set_field
           {
             var = compile_get_local_variable stack_frame var;
             index = Raw_c (string_of_int index);
             value = compile_get_local_variable stack_frame value;
           }
  | Code.Array_set (var, index, value) ->
      Cir_program.add cir
      @@ Set_field
           {
             var = compile_get_local_variable stack_frame var;
             index =
               Type_val (`Int, compile_get_local_variable stack_frame index);
             value = compile_get_local_variable stack_frame value;
           }
  | Code.Event _ -> failwith "unsupported event"

(** Translates a literal constant into its CIR equivalent. *)
and compile_constant context constant =
  match (constant : Code.constant) with
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
  | Float_array _ -> failwith "UNSUPPORTED_FLOAT_ARRAY"

(** Translates an IR expression into CIR. *)
and compile_expression context stack_frame expression =
  match expression with
  | Code.Constant constant -> Constanta (compile_constant context constant)
  | Code.Field (variable, index, _) ->
      Cir.Field_var
        { var = compile_get_local_variable stack_frame variable; slot = index }
  | Code.Block (tag, fields, _, _) ->
      let fields =
        Array.to_list fields
        |> List.map (compile_get_local_variable stack_frame)
      in
      Cir.Block { tag; fields }
  | Code.Closure _ -> assert false
  | Code.Apply { f; args; _ } ->
      let fn = compile_get_local_variable stack_frame f in
      let args = List.map (compile_get_local_variable stack_frame) args in
      Cir.Call { fn; args }
  | Special (Alias_prim _) -> Cir.Raw_c "Val_unit"
  | Prim (prim, args) -> compile_primitive context stack_frame prim args

(** Compiles a primitive to CIR. *)
and compile_primitive context stack_frame prim args =
  let arg = compile_argument context stack_frame in

  match (prim, args) with
  | Code.Vectlength, [ x ] -> Cir.Type_val (`Int, arg x)
  | Array_get, [ array; index ] ->
      let var = arg array in
      let slot = Cir.Type_val (`Int, arg index) in

      Cir.Field_expr { var; slot }
  | Code.Extern "%undefined", _ -> Raw_c "Val_unit"
  | Code.Extern name, [ lhs; rhs ]
    when String.starts_with ~prefix:"%" name
         && List.mem_assoc name integer_binary_operations ->
      compile_integer_binary_operations name (arg lhs) (arg rhs)
  | Code.Extern name, _ when String.starts_with ~prefix:"%" name ->
      compile_extern_builtins context stack_frame name args
  | Code.Extern name, _ -> compile_extern context stack_frame name args
  | Not, [ x ] -> Cir.Val_type (`Bool, Not (Type_val (`Bool, arg x)))
  | IsInt, [ x ] -> Cir.Val_type (`Bool, Is_int (arg x))
  | Eq, [ lhs; rhs ] ->
      Cir.Val_type
        (`Bool, Equal (Type_val (`Int, arg lhs), Type_val (`Int, arg rhs)))
  | Neq, [ lhs; rhs ] -> Cir.Val_type (`Bool, Not (Equal (arg lhs, arg rhs)))
  | Lt, [ lhs; rhs ] -> Cir.Val_type (`Bool, Less_than (arg lhs, arg rhs))
  | Le, [ lhs; rhs ] ->
      Cir.Val_type (`Bool, Less_than_or_equal (arg lhs, arg rhs))
  | _ -> Raw_c "/* unhandled */"

(** Translates binary integer arithmetic and bitwise primitives to CIR binary
    operations. *)
and compile_integer_binary_operations operation lhs rhs =
  let op = List.assoc ~eq:String.equal operation integer_binary_operations in
  let lhs = Cir.Type_val (`Int, lhs) and rhs = Cir.Type_val (`Int, rhs) in

  Cir.Val_type (`Int, Binary_operation (op, lhs, rhs))

(** Compiles built-in compiler primitives prefixed with [%] into equivalent CIR
    expressions. *)
and compile_extern_builtins context stack_frame name args =
  let arg = compile_argument context stack_frame in

  match (name, args) with
  | "%caml_format_int_special", [ x ] ->
      Cir.Call_extern
        { name = "caml_format_int"; args = [ Raw_c "%%d"; arg x ] }
  | "%direct_obj_tag", [ x ] -> Cir.Val_type (`Int, Get_block (`Tag, arg x))
  | "%int_neg", [ x ] -> Cir.Val_type (`Int, Negative (Type_val (`Int, arg x)))
  | "%int_lsr", [ lhs; rhs ] ->
      Cir.Val_type
        ( `Int,
          Binary_operation
            (">>", Type_val (`Int, arg lhs), Type_val (`Int, arg rhs)) )
  | _ -> failwith name

(** Compiles external function calls or C runtime primitives into CIR
    expressions. *)
and compile_extern context stack_frame name args =
  let arg = compile_argument context stack_frame in

  match (name, args) with
  | "caml_array_unsafe_get", [ array; index ] ->
      Cir.Field_expr { var = arg array; slot = Type_val (`Int, arg index) }
  | _ -> Cir.Call_extern { name; args = List.map arg args }

(** Compiles a primitive operand argument (variable or literal constant) into a
    CIR expression. *)
and compile_argument (context : Context.t) (stack_frame : Stack_frame.t) =
  function
  | Code.Pv variable -> compile_get_local_variable stack_frame variable
  | Pc constant -> Cir.Constanta (compile_constant context constant)

let compile_program program =
  let context = Context.make program in
  let cir_program = Cir_program.create () in

  Hashtbl.iter (compile_closure cir_program context) context.closures;

  (context, cir_program, context.string_interner)
