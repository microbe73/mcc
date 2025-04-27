structure AST = struct
  datatype operator
  = Negation
  | Complement
  | Not

  datatype exp
  = Const of int
  | UnOp of operator * exp

  datatype statement = Return of exp

  datatype func = Fun of string * statement

  datatype prog = Prog of func list
end
