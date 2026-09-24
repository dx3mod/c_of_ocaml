module Code = Js_of_ocaml_compiler.Code

module Code_var = struct
  include Code.Var

  let pp = print
end

type instruction =
  | Variable_declaration of string
  | Variable_definition of string * expression
  | Function_declaration of { name : string; argument_count : int }
  | Closure_definition of string * instruction list
  | Set_stack_frame_variable of int * expression
  | Set_variable of Code_var.t * expression
  | Reserve_stack_size of int

and expression =
  | Constanta of constanta
  | Apply of expression * expression list
  | Get_stack_frame_variable of int
  | Get_variable of Code_var.t
  | Field of Code_var.t * int
  | Block of { tag : int; fields : expression list }
  | Call of { f : expression; args : expression list }
  | Call_extern of { function_name : string; arguments : expression list }
  | To_int of expression

and constanta =
  | Int of int
  | Tuple of { tag : int; constants : constanta list }
  | Raw_c of string
[@@deriving show]
