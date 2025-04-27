structure Token = struct
  datatype keyword
  = Int
  | Return

  datatype token
  = Semcol
  | OPar
  | CPar
  | OBrac
  | CBrac
  | Identifier of string
  | KW of keyword
  | IntLiteral of int
  | WS
  | Negation
  | Not
  | Complement

end
  
