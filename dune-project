(lang dune 3.14)

(using experimental_building_ocaml_compiler_with_dune 0.1)

(name c_of_ocaml)

(generate_opam_files true)

(source
 (github nathanfarlow/c-of-ocaml))

(authors "Nathan Farlow" "Mikhail Lopatin <dx3mod@bk.ru>")

(maintainers "Nathan Farlow" "Mikhail Lopatin <dx3mod@bk.ru>")

(license LGPL-2.1)

(package
 (name c_of_ocaml)
 (synopsis "A compiler from OCaml bytecode to standalone ANSI C")
 (description
  "A compiler from OCaml bytecode to standalone ANSI C with an embedded freestanding runtime")
 (depends
  (ocaml
   (= 5.5.1))
  dune
  (js_of_ocaml-compiler
   (= 6.4.1))
  containers
  containers-data
  iter
  ppx_deriving)
 (tags
  (compiler embedded)))
