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
    | FunCall of string * exp list

  datatype statement
    = Return of exp
    | Exp of exp option
    | If of exp * statement * (statement option)
    | Compound of block_item list
    | While of exp * statement
    | Do of statement * exp
    | Break
    | Continue
  and block_item
    = Statement of statement
    | Declaration of declaration
  and declaration
    = Declare of typ * string * (exp option)

  datatype func =
    Fun of string * ((string * typ) list) * (block_item list option) * typ
    (*Name, params, param types, body if not declaration, return type*)

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
