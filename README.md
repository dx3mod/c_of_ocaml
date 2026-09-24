# c_of_ocaml

A compiler from OCaml bytecode to standalone ANSI C with an embedded freestanding runtime.

## Quick start

To build this project, follow these instructions:
```console
$ opam switch create .
$ dune build
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