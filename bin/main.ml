open Cmdliner

let input_file =
  let doc = "Input OCaml bytecode executable (.bc)." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"INPUT_FILE" ~doc)

let disable_optimization =
  let doc = "Disable optimization." in
  Arg.(value & flag & info [ "d"; "disable-optimization" ] ~doc)

let dump_cir =
  let doc = "Dump CIR." in
  Arg.(value & flag & info [ "dump-cir" ] ~doc)

let output_file =
  let doc = "Output C file. Defaults to stdout." in
  Arg.(
    value
    & opt (some file) None
    & info [ "o"; "output" ] ~docv:"OUTPUT_FILE" ~doc)

let extra_c_files =
  let doc =
    "Additional C file to concatenate to the output. Can be given multiple \
     times."
  in
  Arg.(value & opt_all file [] & info [ "c" ] ~docv:"C_FILE" ~doc)

let runtime_variant =
  let doc = "default / avr" in
  Arg.(
    value & opt string "default"
    & info [ "runtime" ] ~docv:"RUNTIME_VARIANT" ~doc)

let run input_file runtime_variant disable_optimization dump_cir output_file
    extra_c_files =
  Printexc.record_backtrace true;

  In_channel.with_open_text input_file @@ fun ic ->
  C_of_ocaml_lib.Pipeline.run ~runtime_variant ~disable_optimization ~dump_cir
    ?output_file ~extra_c_files ic

let cmd =
  let doc = "A transpiler from OCaml to standalone ANSI C file." in
  let info = Cmd.info "c_of_ocaml" ~doc in
  Cmd.v info
    Term.(
      const run $ input_file $ runtime_variant $ disable_optimization $ dump_cir
      $ output_file $ extra_c_files)

let () = exit (Cmd.eval cmd)
