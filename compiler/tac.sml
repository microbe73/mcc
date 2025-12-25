structure TAC : sig
  type quadruple
  type oper
  type arg
  val convert : AST.prog -> quadruple list
end = struct

  structure VM = VarMap
  datatype oper
  = UnOp of AST.un_operator
  | BinOp of AST.bin_operator
  | Call of string * int
  | Param of string
  | Goto
  | CondGoto of AST.bin_operator

  datatype arg
  = Var of string
  | Const of int * AST.typ
  | Target of string

  type quadruple = {label : string option, o : oper, arg1 : arg, arg2 : arg
  option, result : arg option }

  fun convert (prog : AST.prog) : quadruple list = []

end
