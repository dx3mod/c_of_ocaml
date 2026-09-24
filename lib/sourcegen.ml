open Containers
module Code = Js_of_ocaml_compiler.Code

let var_name var = Code.Var.get_name var |> Option.value ~default:"NO_VAR"

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
  | Cir.Reserve_stack_size size ->
      Format.fprintf ppf "reserve_stack_size(%d);" size
  | Cir.Set_variable (variable, expression) ->
      Format.fprintf ppf "%s = " (var_name variable);
      gen_expression ppf expression;
      Format.fprintf ppf ";"
  | _ -> failwith "NOE"

and gen_expression ppf expression =
  match expression with
  | Cir.Constanta constanta -> gen_constanta ppf constanta
  | Cir.Get_stack_frame_variable slot -> Format.fprintf ppf "bp[%d]" slot
  | Cir.Field (variable, index) ->
      Format.fprintf ppf "Field(v_%s, %d)" (var_name variable) index
  | Cir.Block { tag; fields } ->
      Format.fprintf ppf "alloc_block(%d, %d, " tag (List.length fields);
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

and gen_constanta ppf constanta =
  match constanta with
  | Cir.Int x -> Format.fprintf ppf "Val_int(%dL)" x
  | Cir.Raw_c raw -> Format.pp_print_string ppf raw
  | Cir.Tuple { tag; constants } ->
      Format.fprintf ppf "alloc_tuple(%d, %d, " tag (List.length constants);
      gen_args_list ppf gen_constanta constants;
      Format.fprintf ppf ")"

let compile_to_string (cir, _string_interner) =
  let ppf = Format.get_std_formatter () in

  Dynarray.iter (gen_instruction ppf) cir;

  Format.flush_str_formatter ()
