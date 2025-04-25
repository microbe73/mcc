structure AST = struct
  datatype exp = Const of int
  datatype statement = Return of exp
  datatype func = Fun of string * statement
  datatype prog = Prog of func list
end
