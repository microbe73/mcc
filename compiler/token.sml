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
  | For
  | While
  | Do
  | Break
  | Continue

  fun toString (tok : token) : string =
    (case tok
      of Semcol => ";"
       | OPar => "("
       | Return => "return"
       | CPar => ")"
       | OBrac => "{"
       | CBrac => "}"
       | Identifier s => "Var: " ^ s
       | TyID t => "Int"
       | IntLiteral n => Int.toString n
       | WS => "  "
       | Minus => "-"
       | Not => "!"
       | Complement => "~"
       | Plus => "+"
       | Times => "*"
       | Div => "/"
       | AND => "&&"
       | OR => "||"
       | Eq => "=="
       | Neq => "!="
       | Lt => "<"
       | Leq => "<="
       | Geq => ">="
       | Gt => ">"
       | BAnd => "&"
       | BXor => "^"
       | BOr => "|"
       | BRight => ">>"
       | BLeft => "<<"
       | Mod => "%"
       | Asgn => "="
       | If => "if"
       | Else => "else"
       | Colon => ":"
       | Question => "?"
       | For => "for"
       | While => "while"
       | Do => "do"
       | Break => "break"
       | Continue => "continue"
    )
end
  
