open Cmdliner

let input_file =
  let doc = "Input OCaml bytecode executable (.bc)." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"INPUT_FILE" ~doc)

let run input_file =
  let bytecode =
    In_channel.with_open_bin input_file @@ fun ic ->
    Js_of_ocaml_compiler.Parse_bytecode.from_exe ~linkall:false ~link_info:false
      ~include_cmis:false ic
  in

  let xinstr_to_string _ xinstr =
    match xinstr with
    | Js_of_ocaml_compiler.Code.Print.Instr instr ->
        Format.asprintf "%a" Js_of_ocaml_compiler.Code.Print.instr instr
    | Js_of_ocaml_compiler.Code.Print.Last last ->
        Format.asprintf "%a" Js_of_ocaml_compiler.Code.Print.last last
  in
  Js_of_ocaml_compiler.Code.Print.program xinstr_to_string bytecode.code

let cmd =
  let doc = "Dump Ocaml bytecode from *.bc file." in
  let info = Cmd.info "dumpbyte" ~doc in
  Cmd.v info Term.(const run $ input_file)

let () = exit (Cmd.eval cmd)
