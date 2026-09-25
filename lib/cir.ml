type instruction =
  (* Declarations *)
  | Variable_declaration of { name : string }
  | Function_declaration of { name : string; argc : int }
  (* Definitions *)
  | Variable_definition of { name : string; value : expression }
  | Closure_definition of { name : string; branch : instruction list }
  (* Stack frame instructions *)
  | Reserve_stack_size of int
  | Set_stack_frame_variable of { slot : int; value : expression }
  | Set_variable of { var : Code.Var.t; value : expression }
  (* Block value instructions *)
  | Set_field of { var : expression; index : expression; value : expression }
  | Add_closure_argument of { var : expression; arg : expression }
  | Offset_ref of { var : expression; n : int }
  (* Flow control instructions *)
  | Label of Code.Addr.t
  | Goto of Code.Addr.t
  | Return of expression
  | Condition of {
      condition : expression;
      then_branch : instruction list;
      else_branch : instruction list;
    }
  | Switch of {
      condition : expression;
      case_branches : (int * instruction list) list;
    }
  (* Exceptions instructions *)
  | Raise of expression
  | Push_trap of {
      branch : instruction list;
      handler_branch : instruction list;
    }
  | Pop_trap

and expression =
  (* Stack frame operations *)
  | Get_stack_frame_variable of { slot : int }
  | Get_variable of Code.Var.t
  (* Function calls *)
  | Call of { fn : expression; args : expression list }
  | Call_extern of { name : string; args : expression list }
  (* Conversions *)
  | Type_val of [ `Int | `Bool ] * expression
  | Val_type of [ `Int | `Bool ] * expression
  (* Values operations *)
  | Constanta of constanta
  | Field_var of { var : expression; slot : int }
  | Field_expr of { var : expression; slot : expression }
  | Block of { tag : int; fields : expression list }
  | Get_block of [ `Tag ] * expression
  (* Closure *)
  | Closure of { name : string; arity : int; fvc : int }
  (* Misc operations *)
  | Not of expression
  | Is_int of expression
  | Equal of expression * expression
  | Less_than of expression * expression
  | Less_than_or_equal of expression * expression
  | Binary_operation of string * expression * expression
  | Negative of expression
  | Raw_c of string

and constanta =
  | Int of int
  | Bool of bool
  | String of string
  | Float of float
  | Int64 of int64
  | Tuple of { tag : int; constants : constanta list }
[@@deriving show]
