open Containers
module Code = Js_of_ocaml_compiler.Code

let var_name var =
  Code.Var.get_name var
  |> Option.get_lazy (fun () ->
      let buffer = Buffer.create 10 in
      let ppf = Format.formatter_of_buffer buffer in
      Code.Var.print ppf var;
      Format.pp_print_flush ppf ();
      Buffer.contents buffer)

let string_name s = Printf.sprintf "s_%d" @@ String.hash s

let gen_args_list ppf pp list =
  let length = List.length list - 1 in
  List.iteri
    (fun i x ->
      pp ppf x;
      if i < length then Format.pp_print_string ppf ", ")
    list

let rec gen_instruction ppf instruction =
  match instruction with
  | Cir.Variable_declaration { name } -> Format.fprintf ppf "value %s;" name
  | Cir.Variable_definition { name; value } ->
      Format.fprintf ppf "%s = " name;
      gen_expression ppf value;
      Format.fprintf ppf ";"
  | Cir.Closure_definition { name; branch } ->
      Format.fprintf ppf "value %s(value* env) {" name;
      List.iter (gen_instruction ppf) branch;
      Format.fprintf ppf "}"
  | Cir.Set_stack_frame_variable { slot; value } ->
      Format.fprintf ppf "bp[%d] = " slot;
      gen_expression ppf value;
      Format.fprintf ppf ";"
  | Cir.Reserve_stack_size size -> Format.fprintf ppf "reserve_stack(%d);" size
  | Cir.Set_variable { var; value } ->
      Format.fprintf ppf "%s = " (var_name var);
      gen_expression ppf value;
      Format.fprintf ppf ";"
  | Cir.Function_declaration { name; argc } ->
      Format.fprintf ppf "value %s(%s);" name
      @@ (List.init argc Fun.(const "value") |> String.concat ", ")
  | Cir.Label label_address -> Format.fprintf ppf "b%d: ;" label_address
  | Cir.Add_closure_argument { var; arg } ->
      Format.fprintf ppf "add_arg(";
      gen_expression ppf var;
      Format.fprintf ppf ", ";
      gen_expression ppf arg;
      Format.fprintf ppf ");"
  | Cir.Goto label_address -> Format.fprintf ppf "goto b%d;" label_address
  | Cir.Return expression ->
      Format.fprintf ppf "return ";
      gen_expression ppf expression;
      Format.fprintf ppf ";"
  | Cir.Raise expression ->
      Format.fprintf ppf "caml_raise(";
      gen_expression ppf expression;
      Format.fprintf ppf ");"
  | Cir.Condition { condition; then_branch; else_branch } ->
      Format.fprintf ppf "if (";
      gen_expression ppf condition;
      Format.fprintf ppf ") {";
      List.iter (gen_instruction ppf) then_branch;
      Format.fprintf ppf "} else {";
      List.iter (gen_instruction ppf) else_branch;
      Format.fprintf ppf "}"
  | Cir.Set_field { var; index; value } ->
      Format.fprintf ppf "Field(";
      gen_expression ppf var;
      Format.fprintf ppf ", ";
      gen_expression ppf index;
      Format.fprintf ppf ") = ";
      gen_expression ppf value;
      Format.fprintf ppf ";"
  | Cir.Offset_ref { var; n } ->
      Format.fprintf ppf "Field(";
      gen_expression ppf var;
      Format.fprintf ppf ", 0";
      Format.fprintf ppf ") += %d;" n
  | Cir.Switch { condition; case_branches } ->
      Format.fprintf ppf " switch (";
      gen_expression ppf condition;
      Format.fprintf ppf ") {";
      List.iter
        (fun (i, case) ->
          Format.fprintf ppf "case %d: {" i;
          List.iter (gen_instruction ppf) case;
          Format.fprintf ppf "}")
        case_branches;
      Format.fprintf ppf "} "
  | Cir.Push_trap { branch; handler_branch } ->
      Format.fprintf ppf
        {| check_trap_stack(); trap_sp->sp = sp; trap_sp->bp = bp; if (setjmp(trap_sp->buf) == 0) { trap_sp++;  |};
      List.iter (gen_instruction ppf) branch;
      Format.fprintf ppf {| } else { |};
      List.iter (gen_instruction ppf) handler_branch;
      Format.fprintf ppf {| } |}
  | Cir.Pop_trap -> Format.fprintf ppf "trap_sp--;"

and gen_expression ppf expression =
  match expression with
  | Cir.Constanta constanta -> gen_constanta ppf constanta
  | Cir.Get_stack_frame_variable { slot } -> Format.fprintf ppf "bp[%d]" slot
  | Cir.Field_var { var; slot } ->
      Format.fprintf ppf "Field(";
      gen_expression ppf var;
      Format.fprintf ppf ", %d)" slot
  | Cir.Field_expr { var; slot } ->
      Format.fprintf ppf "Field(";
      gen_expression ppf var;
      Format.fprintf ppf ", ";
      gen_expression ppf slot;
      Format.fprintf ppf ")"
  | Cir.Block { tag; fields } ->
      Format.fprintf ppf "caml_alloc(%d, %d, " tag (List.length fields);
      (match fields with
      | [] -> Format.fprintf ppf "NULL"
      | _ -> gen_args_list ppf gen_expression fields);
      Format.fprintf ppf ")"
  | Cir.Call { fn; args } ->
      Format.fprintf ppf "caml_call(";
      gen_expression ppf fn;
      Format.fprintf ppf ", %d" (List.length args);
      if not (List.is_empty args) then begin
        Format.fprintf ppf ", ";
        gen_args_list ppf gen_expression args
      end;
      Format.fprintf ppf ")"
  | Cir.Call_extern { name; args } ->
      Format.fprintf ppf "%s(" name;
      gen_args_list ppf gen_expression args;
      Format.fprintf ppf ")"
  | Cir.Val_type (conversion_type, expression) ->
      begin match conversion_type with
      | `Int -> Format.pp_print_string ppf "Val_int("
      | `Bool -> Format.pp_print_string ppf "Val_bool("
      end;
      gen_expression ppf expression;
      Format.pp_print_string ppf ")"
  | Cir.Type_val (conversion_type, expression) ->
      begin match conversion_type with
      | `Int -> Format.pp_print_string ppf "Int_val("
      | `Bool -> Format.pp_print_string ppf "Bool_val("
      end;
      gen_expression ppf expression;
      Format.pp_print_string ppf ")"
  | Cir.Get_variable variable -> Format.fprintf ppf "v_%s" (var_name variable)
  | Cir.Closure { name; arity; fvc } ->
      Format.fprintf ppf "caml_alloc_closure(%s, %d, %d)" name arity fvc
  | Cir.Not expression ->
      Format.fprintf ppf "!(";
      gen_expression ppf expression;
      Format.fprintf ppf ")"
  | Cir.Equal (first_operand, second_operand) ->
      Format.fprintf ppf "(";
      gen_expression ppf first_operand;
      Format.fprintf ppf "==";
      gen_expression ppf second_operand;
      Format.fprintf ppf ")"
  | Cir.Less_than (first_operand, second_operand) ->
      Format.fprintf ppf "(";
      gen_expression ppf first_operand;
      Format.fprintf ppf "<";
      gen_expression ppf second_operand;
      Format.fprintf ppf ")"
  | Cir.Less_than_or_equal (first_operand, second_operand) ->
      Format.fprintf ppf "(";
      gen_expression ppf first_operand;
      Format.fprintf ppf "<=";
      gen_expression ppf second_operand;
      Format.fprintf ppf ")"
  | Cir.Is_int expression ->
      Format.fprintf ppf "Is_int(";
      gen_expression ppf expression;
      Format.fprintf ppf ")"
  | Cir.Binary_operation (operation, first_operand, second_operand) ->
      Format.fprintf ppf "(";
      gen_expression ppf first_operand;
      Format.pp_print_string ppf operation;
      gen_expression ppf second_operand;
      Format.fprintf ppf ")"
  | Cir.Get_block (`Tag, expression) ->
      Format.fprintf ppf "Tag_val(";
      gen_expression ppf expression;
      Format.fprintf ppf ")"
  | Cir.Negative expression ->
      Format.fprintf ppf "(-(";
      gen_expression ppf expression;
      Format.fprintf ppf "))"
  | Cir.Raw_c raw -> Format.pp_print_string ppf raw

and gen_constanta ppf constanta =
  match constanta with
  | Cir.Int x -> Format.fprintf ppf "Val_int(%dL)" x
  | Cir.Int64 x -> Format.fprintf ppf "caml_copy_int64(%LdLL)" x
  | Cir.Bool x -> Format.fprintf ppf "Val_bool(%b)" x
  | Cir.Tuple { tag; constants } ->
      Format.fprintf ppf "caml_alloc(%d, %d, " tag (List.length constants);
      gen_args_list ppf gen_constanta constants;
      Format.fprintf ppf ")"
  | Cir.String s -> Format.fprintf ppf "s_%d" @@ String.hash s
  | Cir.Float f -> Format.fprintf ppf "caml_copy_double(%h)" f

let compile_into_formatter ppf (context, cir, string_interner) extra_c_files =
  let string_constants = Compiler.String_interner.to_iter string_interner in

  Format.pp_print_string ppf
    "\n\n/****************************************************/\n\n\n";

  Format.pp_print_string ppf extra_c_files;
  Format.pp_print_string ppf
    "\n\n/****************************************************/\n\n\n";

  Iter.iter
    (fun s -> Format.fprintf ppf "static value %s;" @@ string_name s)
    string_constants;

  Hashtbl.iter
    (fun label_address _ ->
      Format.fprintf ppf "static value c%d(value*);" label_address)
    context.Compiler.Context.closures;

  Dynarray.iter (gen_instruction ppf) cir;

  Format.fprintf ppf "int main(void) {";

  Format.fprintf ppf "check_stack(%d);" (Iter.length string_constants);

  Iter.iter
    (fun s ->
      Format.fprintf ppf "%s = caml_copy_string(%S);" (string_name s) s;
      Format.fprintf ppf "*(sp++) = %s;" @@ string_name s)
    string_constants;

  Format.fprintf ppf "bp = sp;";
  Format.fprintf ppf "c%d(NULL); return 0;"
    context.Compiler.Context.program.start;

  Format.fprintf ppf "}"
