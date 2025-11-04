structure TAC : sig
  type node
  val convert : AST.prog -> node list
end = struct
  datatype node
  = BinOp of string * string * AST.bin_operator * string
  | UnOp of string * AST.un_operator * string

  val convert (ast : AST.prog) = []

