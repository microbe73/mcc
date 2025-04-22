structure AST = struct
  datatype exp = Const of int
  datatype statement = Return of exp
  datatype fun_decl = Fun of string * statement
  datatype prog = Prog of fun_decl
end
