let run ic =
  let bytecode =
    Js_of_ocaml_compiler.Parse_bytecode.from_exe ~linkall:false ~link_info:false
      ~include_cmis:false ic
  in

  bytecode.code
  (* |> Optimizer.optimizes_program  *)
  |> Compiler.compile_program
  |> Sourcegen.compile_to_string
(* |> ( ^ ) (Runtime_c_code.code ^ "\n\n\n") *)
