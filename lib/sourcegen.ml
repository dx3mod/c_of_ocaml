open Containers
module Code = Js_of_ocaml_compiler.Code

let gen_args_list ppf pp list =
  let length = List.length list - 1 in
  List.iteri
    (fun i x ->
      pp ppf x;
      if i < length then Format.pp_print_string ppf ", ")
    list

let rec gen_instruction ppf instruction =
  match instruction with
  | Cir.Variable_declaration name -> Format.fprintf ppf "void %s;" name
  | Cir.Variable_definition (name, _expression) ->
      Format.fprintf ppf "%s = " name
  | Cir.Closure_definition (name, body) ->
      Format.fprintf ppf "value %s(value* env) {" name;
      List.iter (gen_instruction ppf) body;
      Format.fprintf ppf "}"
  | Cir.Set_stack_frame_variable (_variable, slot, expression) ->
      Format.fprintf ppf "bp[%d] = " slot;
      gen_expression ppf expression;
      Format.fprintf ppf ";"
  | Cir.Reserve_stack_size size ->
      Format.fprintf ppf "reserve_stack_size(%d);" size
  | _ -> failwith ""

and gen_expression ppf expression =
  match expression with
  | Cir.Constanta constanta -> gen_constanta ppf constanta
  | Cir.Get_stack_frame_variable slot -> Format.fprintf ppf "bp[%d]" slot
  | Cir.Field (variable, index) ->
      Format.fprintf ppf "Field(v_%s, %d)"
        (Code.Var.get_name variable |> Option.get)
        index
  | Cir.Block { tag; fields } ->
      Format.fprintf ppf "alloc_block(%d, " tag;
      gen_args_list ppf
        (fun ppf field ->
          Format.pp_print_string ppf (Code.Var.get_name field |> Option.get))
        fields;
      Format.fprintf ppf ")"
  | Cir.Call { f; args } ->
      Format.fprintf ppf "caml_call(v_%s, " (Code.Var.get_name f |> Option.get);
      gen_args_list ppf
        (fun ppf arg ->
          Format.pp_print_string ppf (Code.Var.get_name arg |> Option.get))
        args;
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
