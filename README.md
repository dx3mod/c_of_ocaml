# c_of_ocaml 🐪🤓

Compiles an OCaml program to a standalone ANSI C file. Here's a spinny cube
program running on a calculator. Its OCaml source is [here](calc/cube).

## Features
- ✅ Garbage collector
- ✅ Random selections of the stdlib
- ✅ Exceptions
- ❌ Floats
- ❌ Objects

## Usage

Use opam to create a switch and install the deps for this repo:

```
opam switch create c_of_ocaml 5.2.0
opam pin add js_of_ocaml-compiler 5.8.2
opam install core_unix ppx_jane core async expect_test_helpers_async

# To use this switch:
opam switch c_of_ocaml
```