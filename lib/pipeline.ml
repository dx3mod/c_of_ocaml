open Containers

let initialize_configuration () =
  Js_of_ocaml_compiler.Config.set_target `JavaScript;
  Js_of_ocaml_compiler.Config.set_effects_backend `Disabled;

  Js_of_ocaml_compiler.Targetint.set_num_bits 32

let read_bytecode_from_channel ic =
  Js_of_ocaml_compiler.Parse_bytecode.from_exe ~linkall:false ~link_info:false
    ~include_cmis:false ic

let run ~disable_optimization ~dump_cir ?output_file ~extra_c_files ic =
  initialize_configuration ();

  let program = (read_bytecode_from_channel ic).code in
  let optimized_program =
    if disable_optimization then program
    else Optimizer.optimizes_program program
  in
  let compiled_program = Compiler.compile_program optimized_program in

  let formatter =
    match output_file with
    | None -> Format.std_formatter
    | Some output_file ->
        Format.formatter_of_out_channel @@ open_out output_file
  in

  if dump_cir then
    let _, cir, _ = compiled_program in
    Dynarray.iter Cir.(pp_instruction formatter) cir
  else
    let extra_c_files =
      List.map (fun filename -> IO.(with_in filename read_all)) extra_c_files
      |> String.concat "\n\n"
    in

    Sourcegen.compile_into_formatter formatter compiled_program extra_c_files
