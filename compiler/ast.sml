structure AST = struct
  datatype un_operator
  = Negation
  | Complement
  | Not

  datatype bin_operator
    = Minus
    | Plus
    | Times
    | Div
    | AND
    | OR
    | Eq
    | Neq
    | Leq
    | Lt
    | Gt
    | Geq
    | BAnd
    | BXor
    | BOr
    | BLeft
    | BRight
    | Mod

  datatype typ
    = Int

  datatype exp
    = Const of int
    | UnOp of un_operator * exp
    | BinOp of bin_operator * exp * exp
    | Assign of string * exp
    | Var of string
    | Conditional of exp * exp * exp

  datatype statement
    = Return of exp
    | Exp of exp
    | If of exp * statement * (statement option)
    
  datatype declaration
    = Declare of typ * string * (exp option)

  datatype block_item
    = Statement of statement
    | Declaration of declaration
   
  datatype func = Fun of string * (block_item list)

  datatype prog = Prog of func list


  fun unop_str (u : un_operator) : string =
    (case u
       of Negation => "-"
        | Complement => "~"
        | Not => "!"
    )
(*  fun binop_str (b : bin_operator) : string =
    (case b
       of Minus => "-"
        | Plus => "+"
        | Times => "*"
        | Div => "/"
        | AND => "&&"
        | OR => "||"
        | Leq => "<="
        | Lt => "<"
        | Gt => ">"
        | Geq => ">="
        | Eq => "=="
        | Neq => "!="
    )
*)
end
