include Js_of_ocaml_compiler.Code

module Var = struct
  include Var

  let pp = print
end

module Addr = struct
  include Addr

  let pp = Format.pp_print_int
end
