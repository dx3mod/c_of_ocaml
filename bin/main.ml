open Cmdliner

let input_file =
  let doc = "Input OCaml bytecode executable (.bc)." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"INPUT_FILE" ~doc)

let run input_file =
  In_channel.with_open_text input_file @@ fun ic ->
  let output = C_of_ocaml.Driver.go ic in
  print_string output

let cmd =
  let doc = "A transpiler from OCaml to standalone ANSI C file." in
  let info = Cmd.info "c_of_ocaml" ~doc in
  Cmd.v info Term.(const run $ input_file)

let () = exit (Cmd.eval cmd)
