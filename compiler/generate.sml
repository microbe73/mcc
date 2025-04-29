structure Generate = struct

  fun genExp (exp : AST.exp ) : string =
    (case exp
       of AST.Const n => "\tmovl $" ^ Int.toString n ^ ", %rax\n"
        | AST.UnOp (unop, inner_exp) => 
            (case unop
               of AST.Negation =>
                  genExp inner_exp ^ "\tneg  %rax\n"
                | AST.Complement =>
                  genExp inner_exp ^ "\tnot  %rax\n"
                | AST.Not =>
                    genExp inner_exp ^ "\tcmpl  $0, %rax\n" ^
                    "\tmovl  $0, %rax\n" ^ "\tsete  %al"
            )
        | AST.BinOp (binop, t1, t2) => raise Fail "todo"
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
  fun printExp (exp : AST.exp ) : string =
    (case exp
       of AST.Const n => Int.toString n
        | AST.UnOp (unop, exp1) =>
            "(" ^ AST.unop_str unop ^ " " ^ printExp exp1 ^ ")"
        | AST.BinOp (binop, exp1, exp2) =>
            "(" ^ printExp exp1 ^ " " ^ AST.binop_str binop ^ " " ^ printExp
            exp2 ^ ")"
    )

  fun printAST (t : AST.func ) : string =
    (case t
       of AST.Fun (name, statement) =>
            (case statement
               of AST.Return exp =>
                    "Fun " ^ name ^ " Return " ^ printExp exp
            )
    )
end
