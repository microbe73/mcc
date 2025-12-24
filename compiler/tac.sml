structure TAC : sig
  type quadruple
  type oper
  type arg
end = struct

  datatype oper 
  = UnOp of AST.un_operator
  | BinOp of AST.bin_operator
  | Call of string * int
  | Param of string
  | Goto of string
  | CondGoto of string * AST.bin_operator

  datatype arg
  = Var of string
  | Const of int * AST.typ

  type quadruple = string option * oper * arg * arg * arg
  (* optional label *)
end
