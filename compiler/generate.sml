structure Generate = struct

  fun genExp (exp : AST.exp ) : string =
    (case exp
       of AST.Const n => "\t movl $" ^ Int.toString n ^ "\n"
    )
  fun genStatement (b : AST.statement) : string =
    (case b
       of AST.Return exp => (genExp exp) ^ "\tretq"
    )
  fun genBody (b : AST.statement list) : string =
    (case b
       of [] => ""
        | (stm :: rest) => genStatement stm ^ genBody rest
    )
  fun generate (t : AST.func list) : string =
    (case t
       of [] => ""
        | (fnc :: rest) =>
          (case fnc
             of AST.Fun (name, body) =>
                  "\t.globl _" ^ name ^ "\n" ^ genStatement body ^ generate rest
          )
    )
end
