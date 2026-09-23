# c_of_ocaml 🐪🤓

A transpiler from OCaml to standalone ANSI C file.

## Development

To build this project, follow these instructions:
```console
opam switch create c_of_ocaml 5.2.0
opam pin add js_of_ocaml-compiler 5.8.2
opam install core_unix ppx_jane core async expect_test_helpers_async

# To use this switch:
opam switch c_of_ocaml
```