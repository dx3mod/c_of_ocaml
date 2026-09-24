open Containers
module Code = Js_of_ocaml_compiler.Code

let var_name var = Code.Var.get_name var |> Option.value ~default:"NO_VAR"
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
  | Cir.Variable_declaration name -> Format.fprintf ppf "value %s;" name
  | Cir.Variable_definition (name, expression) ->
      Format.fprintf ppf "%s = " name;
      gen_expression ppf expression;
      Format.fprintf ppf ";"
  | Cir.Closure_definition (name, body) ->
      Format.fprintf ppf "value %s(value* env) {" name;
      List.iter (gen_instruction ppf) body;
      Format.fprintf ppf "}"
  | Cir.Set_stack_frame_variable (slot, expression) ->
      Format.fprintf ppf "bp[%d] = " slot;
      gen_expression ppf expression;
      Format.fprintf ppf ";"
  | Cir.Reserve_stack_size size -> Format.fprintf ppf "reserve_stack(%d);" size
  | Cir.Set_variable (variable, expression) ->
      Format.fprintf ppf "%s = " (var_name variable);
      gen_expression ppf expression;
      Format.fprintf ppf ";"
  | Function_declaration (name, count) ->
      Format.fprintf ppf "value %s(%s);" name
      @@ (List.init count Fun.(const "value") |> String.concat ", ")
  | Label label_address -> Format.fprintf ppf "b%d:" label_address
  | Add_closure_argument (closure_expression, expression) ->
      Format.fprintf ppf "add_arg(";
      gen_expression ppf closure_expression;
      Format.fprintf ppf ", ";
      gen_expression ppf expression;
      Format.fprintf ppf ");"
  | Goto label_address -> Format.fprintf ppf "goto b%d;" label_address
  | Return expression ->
      Format.fprintf ppf "return ";
      gen_expression ppf expression;
      Format.fprintf ppf ";"
  | Raise expression ->
      Format.fprintf ppf "caml_raise(";
      gen_expression ppf expression;
      Format.fprintf ppf ";"

and gen_expression ppf expression =
  match expression with
  | Cir.Constanta constanta -> gen_constanta ppf constanta
  | Cir.Get_stack_frame_variable slot -> Format.fprintf ppf "bp[%d]" slot
  | Cir.Field (variable, index) ->
      Format.fprintf ppf "Field(v_%s, %d)" (var_name variable) index
  | Cir.Block { tag; fields } ->
      Format.fprintf ppf "caml_alloc(%d, %d, " tag (List.length fields);
      (match fields with
      | [] -> Format.fprintf ppf "NULL"
      | _ -> gen_args_list ppf gen_expression fields);
      Format.fprintf ppf ")"
  | Cir.Call { f; args } ->
      Format.fprintf ppf "caml_call(";
      gen_expression ppf f;
      Format.fprintf ppf ", ";
      gen_args_list ppf gen_expression args;
      Format.fprintf ppf ")"
  | Cir.Call_extern { function_name; arguments } ->
      Format.fprintf ppf "%s(" function_name;
      gen_args_list ppf gen_expression arguments;
      Format.fprintf ppf ")"
  | Cir.To_int expression ->
      Format.pp_print_string ppf "Int_val(";
      gen_expression ppf expression;
      Format.pp_print_string ppf ")"
  | Cir.Apply _ -> failwith "not implement apply yet"
  | Cir.Get_variable variable -> Format.fprintf ppf "v_%s" (var_name variable)
  | Cir.Closure { name; arity; free_variables_count } ->
      Format.fprintf ppf "caml_alloc_closure(%s, %d, %d)" name arity
        free_variables_count

and gen_constanta ppf constanta =
  match constanta with
  | Cir.Int x -> Format.fprintf ppf "Val_int(%dL)" x
  | Cir.Raw_c raw -> Format.pp_print_string ppf raw
  | Cir.Tuple { tag; constants } ->
      Format.fprintf ppf "caml_alloc(%d, %d, " tag (List.length constants);
      gen_args_list ppf gen_constanta constants;
      Format.fprintf ppf ")"

let compile_to_string (context, cir, string_interner) =
  let ppf = Format.get_std_formatter () in

  let string_constants = Compiler.String_interner.to_iter string_interner in

  Format.pp_print_string ppf Runtime_c_code.code;
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

  Format.fprintf ppf "}";

  Format.flush_str_formatter ()
