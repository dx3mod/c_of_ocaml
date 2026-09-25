```
a'!   _,,_ a'!   _,,_     a'!   _,,_____
  \\_/ c  \  \\_/ of \      \\_/  ocaml \.-,
   \, /-( /'-,\, /-( /'-,    \, /-----( /
   //\ //\\   //\ //\\       //\     //\\
```

# C_of_ocaml

A compiler from OCaml bytecode to standalone ANSI C with an embedded freestanding runtime.

This is a research project, so don't expect a production-ready solution you'd actually want to use. Special thanks to Nathan Farlow for providing the foundation that got this project off the ground. This repository is a hard fork of his repo, featuring a heavily reworked compiler and efforts to shrink the runtime footprint for low-resource devices.

## Quick start

To build and install this project, follow these instructions:
```console
$ opam switch create .
$ dune build
# $ opam install .
```

### Hello world

Here is a minimal example demonstrating how to compile an OCaml program into C and build a standalone executable using Dune.

First, set up a project directory (e.g., `demo/`) with the following structure:

```
demo/
├── dune
├── dune-project
└── main.ml
```

Write your OCaml code in `main.ml`:
```ocaml
open Stdlib 

let () = Io.puts "Soluton from C!"
```

Configure Dune to build OCaml bytecode, run `c_of_ocaml` to generate C source code, and compile the result using your system's C compiler:
```
(executables
 (names main)
 (libraries c_of_ocaml.stdlib)
 (flags :standard -nopervasives)
 (modes byte))

(rule
 (targets main.c)
 (action
  (with-stdout-to
   %{targets}
   (run %{bin:c_of_ocaml} %{dep:main.bc}))))

(rule
 (target main.c.exe)
 (deps main.c)
 (action
  (run %{cc} -ansi -Og -g -o %{target} %{deps})))
```

Finally, build and run the generated C binary:
```console
$ dune build
$ ./_build/default/main.c.exe
Soluton from C!
```

## Internals

This project uses [Js_of_ocaml] as a frontend to get and process bytecode, including applying a range of optimizations. Basically, the [pipeline](./lib/pipeline.ml) works like this:

1. Get the bytecode
2. Apply [optimizations](./lib/optimizer.ml) from Js_of_ocaml
3. Compile it into [CIR](./lib/cir.ml), a semantic intermediate representation
4. [Generate C code](./lib/sourcegen.ml) from CIR

## License

Licensed under [LGPL-2.1](./LICENSE). Pull requests are welcome.

[Js_of_ocaml]: https://ocsigen.org/js_of_ocaml/latest/js_of_ocaml/index.html