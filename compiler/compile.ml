open Containers
module Code = Js_of_ocaml_compiler.Code

module Closure = struct
  type info = {
    free_vars : Code.Var.t list;
    cont : Code.cont;
    params : Code.Var.t list;
  }
end

module Context = struct
  module Strings_hash_set = CCHashSet.Make (String)

  type t = {
    program : Code.program;
    closures : closures;
    strings : Strings_hash_set.t;
  }

  and closures = (int, Closure.info) Hashtbl.t

  let find_closures program : closures =
    let free_vars = Js_of_ocaml_compiler.Freevars.f program in

    let map_closure (params, ((pc, _) as cont)) =
      let free_vars =
        Code.Addr.Map.find_opt pc free_vars
        |> Option.map_or ~default:[] Code.Var.Set.elements
      in

      (pc, Closure.{ free_vars; params; cont })
    in

    Code.fold_closures program
      (fun _ params cont acc -> map_closure (params, cont) :: acc)
      []
    |> Hashtbl.of_list
end

let variable_name_to_string name =
  Printf.sprintf "v_%s" @@ Code.Var.to_string name

and closure_name_to_string pc = Printf.sprintf "c%d" pc
and block_name_to_string pc = Printf.sprintf "b%d" pc
and global_string_name_to_string s = Printf.sprintf "s_%s" s

module Frame_stack = struct
  type t = (int, int) Hashtbl.t

  let slot_opt stack v = Hashtbl.find_opt stack @@ Code.Var.idx v

  let get stack v =
    match slot_opt stack v with
    | None -> variable_name_to_string v
    | Some i -> Printf.sprintf "bp[%d]" i

  let set ?(decl = false) stack v exp =
    match slot_opt stack v with
    | Some i -> Printf.sprintf "bp[%d] = %s;" i exp
    | None when decl ->
        Printf.sprintf "value %s = %s;" (variable_name_to_string v) exp
    | _ -> Printf.sprintf "%s = %s;" (variable_name_to_string v) exp
end

let rename_id = ref 0

let next_rename_id () =
  let id = !rename_id in
  incr rename_id;
  id

let rename stack (context : Context.t) pc args =
  let id = next_rename_id () in
  let params = (Code.Addr.Map.find pc context.program.blocks).params in
  let t i = Printf.sprintf "t%d_%d" id i in

  List.mapi
    (fun i a -> Printf.sprintf "value %s = %s" (t i) (Frame_stack.get stack a))
    args
  @ List.mapi (fun i p -> Frame_stack.set stack p (t i)) params
  |> String.concat "\n"

let collect_vars (context : Context.t) pc =
  let module Variables = CCHashSet.Make (Int) in
  let vars = Variables.create 10 in
  let add v = Variables.insert vars (Code.Var.idx v) in

  let fold = Code.fold_children in
  Code.traverse { fold }
    begin fun pc () ->
      let block = Code.Addr.Map.find pc context.program.blocks in

      List.iter add block.Code.params;
      List.iter (function Code.Let (v, _), _ -> add v | _ -> ()) block.body;
      (* Also collect exception variables from Pushtrap *)
      match fst block.branch with
      | Pushtrap (_, exn_var, _) -> add exn_var
      | _ -> ()
    end
    pc context.program.blocks ();

  Variables.to_iter vars |> Iter.mapi (fun i v -> (v, i)) |> Iter.to_hashtbl

let rec compile_closure (context : Context.t) pc (info : Closure.info) =
  let stack = collect_vars context pc in
  (* Ensure free_vars and params have stack slots so GC can update them *)
  List.iter
    (fun v ->
      if not (Hashtbl.mem stack (Code.Var.idx v)) then
        Hashtbl.add stack (Code.Var.idx v) (Hashtbl.length stack))
    (info.free_vars @ info.params);
  (* Используем Hashtbl как множество посещённых блоков *)
  let visited = Hashtbl.create 10 in
  let n = Hashtbl.length stack in
  [
    Printf.sprintf "value %s(value* env)" (closure_name_to_string pc);
    "{";
    Printf.sprintf "reserve_stack(%d);" n;
    List.mapi
      (fun i v ->
        Frame_stack.set ~decl:true stack v (Printf.sprintf "env[%d]" i))
      (info.free_vars @ info.params)
    |> String.concat "\n";
    rename stack context pc (snd info.cont);
    compile_block context visited stack pc;
    "}";
  ]
  |> String.concat "\n"

and compile_block context visited stack pc =
  if Hashtbl.mem visited pc then ""
  else begin
    Hashtbl.add visited pc ();
    let block = Code.Addr.Map.find pc context.program.blocks in
    let body = block.Code.body in
    let branch = block.Code.branch in
    let rec go acc = function
      | [] -> List.rev acc
      | instrs -> (
          let rec take_cls acc = function
            | i :: rest when Option.is_some (closure_of i) ->
                take_cls (i :: acc) rest
            | rest -> (List.rev acc, rest)
          in

          let cls, rest = take_cls [] instrs in
          match List.filter_map closure_of cls with
          | [] -> (
              match rest with
              | [] -> List.rev acc
              | i :: rest -> go (compile_instr context stack i :: acc) rest)
          | infos ->
              let allocs =
                List.map
                  (fun (v, p, pc) ->
                    let fv = (Hashtbl.find context.closures pc).free_vars in
                    Frame_stack.set ~decl:true stack v
                      (Printf.sprintf "caml_alloc_closure(%s, %d, %d);"
                         (closure_name_to_string pc)
                         (List.length p) (List.length fv)))
                  infos
              in
              let fills =
                List.map
                  (fun (v, _, pc) ->
                    let fv = (Hashtbl.find context.closures pc).free_vars in
                    List.map
                      (fun f ->
                        Printf.sprintf "add_arg(%s, %s);"
                          (Frame_stack.get stack v) (Frame_stack.get stack f))
                      fv
                    |> String.concat "\n")
                  infos
              in
              go (List.rev_append (allocs @ fills) acc) rest)
    in
    let instrs =
      go [] body @ [ compile_last context visited stack branch ]
      |> String.concat "\n"
    in
    Printf.sprintf "%s:;\n%s" (block_name_to_string pc) instrs
  end

and closure_of = function
  | Let (v, Closure (p, (pc, _))), _ -> Some (v, p, pc)
  | _ -> None

and compile_instr context stack (instr, _) =
  let g = Frame_stack.get stack in
  match instr with
  | Let (v, Closure (p, (pc, _))) ->
      let fv = (Hashtbl.find context.closures pc).free_vars in
      let alloc =
        Frame_stack.set ~decl:true stack v
          (Printf.sprintf "caml_alloc_closure(%s, %d, %d);"
             (closure_name_to_string pc)
             (List.length p) (List.length fv))
      in
      let fills =
        List.map (fun f -> Printf.sprintf "add_arg(%s, %s);" (g v) (g f)) fv
      in
      alloc :: fills |> String.concat "\n"
  | Let (v, Constant c) ->
      let preamble, expr = compile_const context c in
      String.concat "\n" (preamble @ [ Frame_stack.set ~decl:true stack v expr ])
  | Let (v, e) ->
      Frame_stack.set ~decl:true stack v (compile_expr context stack e)
  | Assign (v1, v2) -> Frame_stack.set stack v1 (g v2)
  | Set_field (v, n, x) -> Printf.sprintf "Field(%s, %d) = %s;" (g v) n (g x)
  | Offset_ref (v, n) -> Printf.sprintf "Field(%s, 0) += %d;" (g v) n
  | Array_set (a, i, x) ->
      Printf.sprintf "Field(%s, Int_val(%s)) = %s;" (g a) (g i) (g x)

and compile_expr context stack = function
  | Apply { f; args; _ } ->
      let a = List.map (Frame_stack.get stack) args |> String.concat ", " in
      Printf.sprintf "caml_call(%s, %d, %s)" (Frame_stack.get stack f)
        (List.length args) a
  | Block (tag, fields, _, _) ->
      let fs =
        Array.to_list fields
        |> List.map (Frame_stack.get stack)
        |> String.concat ", "
      in
      Printf.sprintf "caml_alloc(%d, %d, %s)" tag (Array.length fields) fs
  | Field (v, n) -> Printf.sprintf "Field(%s, %d)" (Frame_stack.get stack v) n
  | Constant c -> snd (compile_const context c)
  | Prim (p, args) -> compile_prim context stack p args
  | Closure _ -> assert false
  | Special Undefined -> "Val_unit"
  | Special (Alias_prim _) -> "Val_unit"

and compile_last context visited stack (last, _) =
  let g = Frame_stack.get stack in
  let branch pc args =
    Printf.sprintf "%s\ngoto %s;"
      (rename stack context pc args)
      (block_name_to_string pc)
  in
  match last with
  | Return v -> Printf.sprintf "return %s;" (g v)
  | Raise (v, _) -> Printf.sprintf "caml_raise(%s);" (g v)
  | Stop -> "return Val_unit;"
  | Branch (pc, args) ->
      let br = branch pc args in
      let block = compile_block context visited stack pc in
      Printf.sprintf "%s\n%s" br block
  | Cond (v, (pc1, a1), (pc2, a2)) ->
      let then_branch = branch pc1 a1 in
      let else_branch = branch pc2 a2 in
      let block1 = compile_block context visited stack pc1 in
      let block2 = compile_block context visited stack pc2 in
      Printf.sprintf "if (Bool_val(%s)) { %s } else { %s }\n%s\n%s" (g v)
        then_branch else_branch block1 block2
  | Switch (v, arr) ->
      let cases =
        Array.mapi
          (fun i (pc, args) ->
            let br = branch pc args in
            let block = compile_block context visited stack pc in
            Printf.sprintf "case %d: %s\n%s" i br block)
          arr
      in
      let cases_str = Array.to_list cases |> String.concat "\n" in
      Printf.sprintf "switch (Int_val(%s)) {\n%s\n}" (g v) cases_str
  | Pushtrap ((body_pc, body_args), exn_var, (handler_pc, handler_args)) ->
      let body_branch = branch body_pc body_args in
      let handler_branch = branch handler_pc handler_args in
      let body_block = compile_block context visited stack body_pc in
      let handler_block = compile_block context visited stack handler_pc in
      Printf.sprintf
        "check_trap_stack();\n\
         trap_sp->sp = sp; trap_sp->bp = bp;\n\
         if (setjmp(trap_sp->buf) == 0) { trap_sp++; %s }\n\
         else { %s %s }\n\
         %s\n\
         %s"
        body_branch
        (Frame_stack.set stack exn_var "exn_value")
        handler_branch body_block handler_block
  | Poptrap (pc, args) ->
      let br = branch pc args in
      let block = compile_block context visited stack pc in
      Printf.sprintf "trap_sp--;\n%s\n%s" br block

and const_allocates : Code.constant -> bool = function
  | Int _ | String _ | NativeString _ -> false
  | Float _ | Int64 _ | Float_array _ | Tuple _ -> true

(* Returns (preamble_statements, expression) *)
and compile_const context c =
  match c with
  | Int i -> ([], Printf.sprintf "Val_int(%ldL)" i)
  | Float f -> ([], Printf.sprintf "caml_copy_double(%h)" f)
  | String s | NativeString (Byte s | Utf (Utf8 s)) ->
      Context.Strings_hash_set.insert context.strings s;
      ([], global_string_name_to_string s)
  | Int64 i -> ([], Printf.sprintf "caml_copy_int64(%LdLL)" i)
  | Float_array fa ->
      let elts =
        Array.to_list fa
        |> List.map (fun f -> Printf.sprintf "%h" f)
        |> String.concat ", "
      in
      ( [],
        Printf.sprintf "caml_alloc_float_array(%d, (double[]){%s})"
          (Array.length fa) elts )
  | Tuple (tag, elts, _) ->
      let preambles, args =
        Array.fold_left
          (fun (preambles, args) e ->
            let preamble, expr = compile_const context e in
            (preambles @ preamble, args @ [ expr ]))
          ([], []) elts
      in
      let args_str = String.concat ", " args in
      ( preambles,
        Printf.sprintf "caml_alloc(%d, %d, %s)" tag (Array.length elts) args_str
      )

and compile_prim context stack prim args =
  let arg = function
    | Code.Pv v -> Frame_stack.get stack v
    | Pc c -> snd (compile_const context c)
  in
  let a, b =
    match args with
    | [ x ] -> (arg x, "")
    | [ x; y ] -> (arg x, arg y)
    | _ -> ("", "")
  in
  match (prim, args) with
  | Vectlength, [ x ] -> Printf.sprintf "Int_val(%s)" (arg x)
  | Array_get, [ arr; i ] ->
      Printf.sprintf "Field(%s, Int_val(%s))" (arg arr) (arg i)
  | Extern "%undefined", _ -> "Val_unit"
  | Extern name, _ -> compile_extern name a b args arg
  | Not, [ _ ] -> Printf.sprintf "Val_bool(!Bool_val(%s))" a
  | IsInt, [ _ ] -> Printf.sprintf "Val_bool(Is_int(%s))" a
  | Eq, [ _; _ ] -> Printf.sprintf "Val_bool(%s == %s)" a b
  | Neq, [ _; _ ] -> Printf.sprintf "Val_bool(%s != %s)" a b
  | Lt, [ _; _ ] -> Printf.sprintf "Val_bool(Int_val(%s) < Int_val(%s))" a b
  | Le, [ _; _ ] -> Printf.sprintf "Val_bool(Int_val(%s) <= Int_val(%s))" a b
  | Ult, [ _; _ ] ->
      Printf.sprintf "Val_bool((uintnat)Int_val(%s) < (uintnat)Int_val(%s))" a b
  | _ -> "/* unhandled */"

and int_binops =
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

and compile_extern name a b args arg =
  match Stdlib.List.assoc_opt name int_binops with
  | Some op -> Printf.sprintf "Val_int(Int_val(%s) %s Int_val(%s))" a op b
  | None -> (
      match name with
      | "%int_lsr" ->
          Printf.sprintf "Val_int((uintnat)Int_val(%s) >> Int_val(%s))" a b
      | "%int_neg" -> Printf.sprintf "Val_int(-Int_val(%s))" a
      | "%caml_format_int_special" ->
          Printf.sprintf "caml_format_int(\"%%d\", %s)" a
      | "%direct_obj_tag" -> Printf.sprintf "Val_int(Tag_val(%s))" a
      | "caml_array_unsafe_get" -> Printf.sprintf "Field(%s, Int_val(%s))" a b
      | _ ->
          let args_str = List.map arg args |> String.concat ", " in
          Printf.sprintf "%s(%s)" name args_str)

let f program =
  let context =
    Context.
      {
        program;
        closures = Context.find_closures program;
        strings = Context.Strings_hash_set.create 10;
      }
  in
  let cls = Hashtbl.fold (fun pc c acc -> (pc, c) :: acc) context.closures [] in
  let bodies = List.map (fun (pc, c) -> compile_closure context pc c) cls in
  let strs = Context.Strings_hash_set.to_iter context.strings |> Iter.to_list in
  [
    List.map
      (fun (pc, _) ->
        Printf.sprintf "value %s(value* env);" (closure_name_to_string pc))
      cls;
    List.map
      (fun s -> Printf.sprintf "value %s;" (global_string_name_to_string s))
      strs;
    bodies;
    [ "int main() {" ];
    (let n = List.length strs in
     if n > 0 then [ Printf.sprintf "check_stack(%d);" n ] else []);
    List.concat_map
      (fun s ->
        [
          Printf.sprintf "%s = caml_copy_string(\"%s\");"
            (global_string_name_to_string s)
            s;
          Printf.sprintf "*(sp++) = %s;" (global_string_name_to_string s);
        ])
      strs;
    [
      Printf.sprintf "bp = sp; %s(NULL); return 0;}"
        (closure_name_to_string program.Code.start);
    ];
  ]
  |> List.concat |> String.concat "\n"
