structure Token = struct
  datatype type_id
  = Int

  datatype token
  = Semcol
  | OPar
  | Return
  | CPar
  | OBrac
  | CBrac
  | Identifier of string
  | TyID of type_id
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
  | BAnd
  | BXor
  | BOr
  | BRight
  | BLeft
  | Mod
  | Asgn
  | If
  | Else
  | Colon
  | Question
end
  
