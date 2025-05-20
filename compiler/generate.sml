structure Generate = struct
  val count = ref 0
  fun incr (a : unit) : int =
    let val _ = count := !count + 1 in !count end

  fun new_label (a : unit) : string =
  let
    val _ = count := !count + 1
  in
    "_label" ^ Int.toString (!count)
  end
      
  fun genExp (exp : AST.exp ) : string =
    (case exp
       of AST.Const n => "\tmovq $" ^ Int.toString n ^ ", %rax\n"
        | AST.UnOp (unop, inner_exp) => 
            (case unop
               of AST.Negation =>
                  genExp inner_exp ^ "\tneg  %rax\n"
                | AST.Complement =>
                  genExp inner_exp ^ "\tnot  %rax\n"
                | AST.Not =>
                    genExp inner_exp ^ "\tcmpq  $0, %rax\n" ^
                    "\tmovq  $0, %rax\n" ^ "\tsete  %al\n"
            )
        | AST.BinOp (binop, e1, e2) =>
          (case binop
            of AST.Plus =>
              genExp e1 ^ "\tpushq %rax\n" ^ genExp e2 ^ 
            "\tpopq %rcx\n" ^ "\tadd %rcx, %rax\n"
            | AST.Minus => 
              genExp e1 ^ "\tpushq %rax\n" ^ genExp e2 ^
            "\tpopq %rcx\n" ^ "\tsub %rax, %rcx\n" ^ "\tmovq %rcx, %rax\n"
            | AST.Times =>
              genExp e1 ^ "\tpushq %rax\n" ^ genExp e2 ^ 
            "\tpopq %rcx\n" ^ "\timul %rcx, %rax\n"
            | AST.Div =>
              genExp e1 ^ "\tpushq %rax\n" ^ genExp e2 ^
              "\tpopq %rcx\n" ^ "\tpushq %rax\n" ^ "\tmovq %rcx, %rax\n"
              ^ "\tpopq %rcx\n" ^ "\tcqo\n" ^ "\tidiv %rcx\n"
            | AST.OR =>
              let
                val clause2 = new_label()
                val end_label = new_label()
              in
               genExp e1 ^ 
               "    cmpq $0, %rax\n" ^ 
               "    je " ^ clause2 ^ "\n" ^
               "    movq $1, %rax\n" ^ 
               "    jmp " ^ end_label ^ "\n" ^
               clause2 ^ ":\n" ^
               genExp e2 ^ 
               "    cmpq $0, %rax\n" ^
               "    movq $0, %rax\n" ^
               "    setne %al\n" ^
               end_label ^ ":\n"
              end
            | AST.AND => 
              let
                val clause2 = new_label()
                val end_label = new_label()
              in
               genExp e1 ^
               "    cmpq $0, %rax\n" ^
               "    jne " ^ clause2 ^ "\n" ^
               "    jmp " ^ end_label ^ "\n" ^
               clause2 ^ ":\n" ^
               genExp e2 ^
               "    cmpq $0, %rax\n" ^
               "    movq $0, %rax\n" ^
               "    setne %al\n" ^
               end_label ^ ":\n"
              end
            | AST.Eq =>
              genExp e1 ^ "\tpushq %rax\n" ^ genExp e2 ^
              "\tpopq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
              ^ "\tsete %al\n"
            | AST.Neq =>
              genExp e1 ^ "\tpushq %rax\n" ^ genExp e2 ^
              "\tpopq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
              ^ "\tsetne %al\n"
            | AST.Leq =>
              genExp e1 ^ "\tpushq %rax\n" ^ genExp e2 ^
              "\tpopq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
              ^ "\tsetle %al\n"
            | AST.Geq =>
              genExp e1 ^ "\tpushq %rax\n" ^ genExp e2 ^
              "\tpopq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
              ^ "\tsetge %al\n"
            | AST.Gt =>
              genExp e1 ^ "\tpushq %rax\n" ^ genExp e2 ^
              "\tpopq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
              ^ "\tsetg %al\n"
            | AST.Lt =>
              genExp e1 ^ "\tpushq %rax\n" ^ genExp e2 ^
              "\tpopq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
              ^ "\tsetl %al\n"
            | AST.BAnd => 
             genExp e1 ^
             "    pushq %rax\n" ^
             genExp e2 ^
             "    popq %rcx\n" ^
             "    and %rcx, %rax\n"
            | AST.BOr => 
             genExp e1 ^
             "    pushq %rax\n" ^
             genExp e2 ^
             "    popq %rcx\n" ^
             "    or %rcx, %rax\n"
            | AST.BXor =>
             genExp e1 ^
             "    pushq %rax\n" ^
             genExp e2 ^
             "    popq %rcx\n" ^
             "    xor %rcx, %rax\n"
            | AST.BLeft =>
             genExp e1 ^
             "    pushq %rax\n" ^
             genExp e2 ^
             "    movq %rax, %rcx\n" ^
             "    popq %rax\n" ^
             "    shlq %cl, %rax\n"
            | AST.BRight =>
             genExp e1 ^
             "    pushq %rax\n" ^
             genExp e2 ^
             "    movq %rax, %rcx\n" ^
             "    popq %rax\n" ^
             "    shrq %cl, %rax\n"
            | AST.Mod =>
              genExp e1 ^ "\tpushq %rax\n" ^ genExp e2 ^
              "\tpopq %rcx\n" ^ "\tpushq %rax\n" ^ "\tmovq %rcx, %rax\n"
              ^ "\tpopq %rcx\n" ^ "\tcqo\n" ^ "\tidiv %rcx\n" ^ 
              "\tmovq %rdx, %rax\n"
          )
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
                  "\t.globl _" ^ name ^ "\n" ^ "_" ^ name ^ ":\n" ^ 
                  genBody body ^ generate rest
          )
    )
(*
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
*)
end
