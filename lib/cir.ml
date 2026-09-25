module Code = Js_of_ocaml_compiler.Code

module Code_var = struct
  include Code.Var

  let pp = print
end

module Code_addr = struct
  include Code.Addr

  let pp = Format.pp_print_int
end

type instruction =
  | Variable_declaration of string
  | Variable_definition of string * expression
  | Function_declaration of string * int
  | Closure_definition of string * instruction list
  | Set_stack_frame_variable of slot * expression
  | Set_variable of Code_var.t * expression
  | Reserve_stack_size of int
  | Label of Code_addr.t
  | Add_closure_argument of expression * expression
  | Return of expression
  | Raise of expression
  | Goto of Code_addr.t
  | Condition of expression * body * body
  | Switch of expression * (int * body) list
  | Set_field of expression * expression * expression
  | Push_trap of { body : body; handler : body }
  | Pop_trap

and expression =
  | Constanta of constanta
  | Apply of expression * expression list
  | Get_stack_frame_variable of slot
  | Get_variable of Code_var.t
  | Field of Code_var.t * int
  | Field' of expression * expression
  | Block of { tag : int; fields : expression list }
  | Call of { f : expression; args : expression list }
  | Call_extern of
      [ `Name of string | `Expression of expression ] * expression list
  | Type_val of [ `Int | `Bool ] * expression
  | Val_type of [ `Int | `Bool ] * expression
  | Closure of { name : string; arity : int; free_variables_count : int }
  | Not of expression
  | Is_int of expression
  | Equal of expression * expression
  | Less_than of expression * expression
  | Less_than_or_equal of expression * expression
  | Binary_operation of string * expression * expression
  | Get_block of [ `Tag ] * expression
  | Negative of expression

and constanta =
  | Int of int
  | Bool of bool
  | String of string
  | Float of float
  | Int64 of int64
  | Tuple of { tag : int; constants : constanta list }
  | Raw_c of string

and body = instruction list
and slot = int [@@deriving show]
