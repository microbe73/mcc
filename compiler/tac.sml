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

  (*https://smlnj.org/doc/smlnj-lib/Util/str-HashTable.html
   * In particular, using the copy function each time a new scope is created can
   * allow for unique symbol tables for each scope [but i don't love that idea]
   * Could also try the one symbol table strategy using findAndRemove
   * In general hash maps in this library work slightly unusually, almost every
   * function just returns a unit (so the tables are basically just usual
   * imperative tables) *)
  type symbol = {name : string, vartype : AST.typ, allocated_space : int}
  fun convert (prog : AST.prog) : quadruple list = []

end
