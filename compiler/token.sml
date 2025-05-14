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
  | Minus
  | Not
  | Complement
  | Plus
  | Times
  | Div
  | AND
  | OR
  | Eq
  | Neq
  | Lt
  | Leq
  | Geq
  | Gt
end
  
