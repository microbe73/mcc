structure Generate = struct
  structure VM = VarMap
  (* pmap maps variables to stack offsets (if types are added it will include
  those as well), and int is the current offset context *)
  type context = (VM.pmap * int)
  val fresh_context : context = (VM.empty_map, 0)




  val count = ref 0
  fun incr (a : unit) : int =
    let val _ = count := !count + 1 in !count end

  fun new_label (a : unit) : string =
  let
    val _ = count := !count + 1
  in
    "_label" ^ Int.toString (!count)
  end

  (*TODO: refactor genExp not to take in a context but only a pmap, it can't
  update the offset since declarations are not expressions. This will also make
  blocks easier I think and just in general *)
      
  fun genExp (exp_w_context : AST.exp * VM.pmap) : string =
    (case exp_w_context of (exp, ctxt) =>
      (case exp
         of AST.Const n => "\tmovq $" ^ Int.toString n ^ ", %rax\n"
          | AST.UnOp (unop, inner_exp) =>
              (case unop
                 of AST.Negation =>
                    (case genExp (inner_exp, ctxt)
                      of new_exp  =>
                        new_exp ^ "\tneg  %rax\n" 
                    )
                  | AST.Complement =>
                    (case genExp (inner_exp, ctxt)
                      of new_exp  =>
                        new_exp ^ "\tnot  %rax\n" 
                    )
                  | AST.Not =>
                    (case genExp (inner_exp, ctxt)
                      of new_exp  =>
                        new_exp ^ "\tcmpq  $0, %rax\n" ^
                      "\tmovq  $0, %rax\n" ^ "\tsete  %al\n"
                       
                    )
               )
          | AST.BinOp (binop, e1, e2) =>
            (case binop
              of AST.Plus =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
               exp1 ^ "\tpushq %rax\n" ^ exp2 ^ 
              "\tpopq %rcx\n" ^ "\tadd %rcx, %rax\n" 
              end
              | AST.Minus =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "\tpushq %rax\n" ^ exp2 ^
              "\tpopq %rcx\n" ^ "\tsub %rax, %rcx\n" ^ "\tmovq %rcx, %rax\n"
              end
              | AST.Times =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "\tpushq %rax\n" ^ exp2 ^ 
              "\tpopq %rcx\n" ^ "\timul %rcx, %rax\n" 
             end 
              | AST.Div =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "\tpushq %rax\n" ^ exp2 ^
                "\tpopq %rcx\n" ^ "\tpushq %rax\n" ^ "\tmovq %rcx, %rax\n"
                ^ "\tpopq %rcx\n" ^ "\tcqo\n" ^ "\tidiv %rcx\n" 
             end 
              | AST.OR =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
                let
                  val clause2 = new_label()
                  val end_label = new_label()
                in
                
                 exp1 ^ 
                 "    cmpq $0, %rax\n" ^ 
                 "    je " ^ clause2 ^ "\n" ^
                 "    movq $1, %rax\n" ^ 
                 "    jmp " ^ end_label ^ "\n" ^
                 clause2 ^ ":\n" ^
                 exp2 ^ 
                 "    cmpq $0, %rax\n" ^
                 "    movq $0, %rax\n" ^
                 "    setne %al\n" ^
                 end_label ^ ":\n" 
                end 
             end 
              | AST.AND =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
                let
                  val clause2 = new_label()
                  val end_label = new_label()
                in
                
                 exp1 ^
                 "    cmpq $0, %rax\n" ^
                 "    jne " ^ clause2 ^ "\n" ^
                 "    jmp " ^ end_label ^ "\n" ^
                 clause2 ^ ":\n" ^
                 exp2 ^
                 "    cmpq $0, %rax\n" ^
                 "    movq $0, %rax\n" ^
                 "    setne %al\n" ^
                 end_label ^ ":\n" 
                end
             end 
              | AST.Eq =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "\tpushq %rax\n" ^ exp2 ^
                "\tpopq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
                ^ "\tsete %al\n" 
             end 
              | AST.Neq =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
                (* val e1 = #1(exp_w_ctxt1)
                val e2 = #1(exp_w_ctxt2)
                val ctxt = #2(exp_w_ctxt2) *)
              in
              exp1 ^ "\tpushq %rax\n" ^ exp2 ^
                "\tpopq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
                ^ "\tsetne %al\n" 
             end 
              | AST.Leq =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
                (* val e1 = #1(exp_w_ctxt1)
                val e2 = #1(exp_w_ctxt2)
                val ctxt = #2(exp_w_ctxt2) *)
              in
              exp1 ^ "\tpushq %rax\n" ^ exp2 ^
                "\tpopq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
                ^ "\tsetle %al\n" 
             end 
              | AST.Geq =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
                (* val e1 = #1(exp_w_ctxt1)
                val e2 = #1(exp_w_ctxt2)
                val ctxt = #2(exp_w_ctxt2) *)
              in
              exp1 ^ "\tpushq %rax\n" ^ exp2 ^
                "\tpopq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
                ^ "\tsetge %al\n" 
             end 
              | AST.Gt =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
                (* val e1 = #1(exp_w_ctxt1)
                val e2 = #1(exp_w_ctxt2)
                val ctxt = #2(exp_w_ctxt2) *)
              in
              exp1 ^ "\tpushq %rax\n" ^ exp2 ^
                "\tpopq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
                ^ "\tsetg %al\n" 
             end 
              | AST.Lt =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
                (* val e1 = #1(exp_w_ctxt1)
                val e2 = #1(exp_w_ctxt2)
                val ctxt = #2(exp_w_ctxt2) *)
              in
              exp1 ^ "\tpushq %rax\n" ^ exp2 ^
                "\tpopq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
                ^ "\tsetl %al\n" 
             end 
              | AST.BAnd =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
                (* val e1 = #1(exp_w_ctxt1)
                val e2 = #1(exp_w_ctxt2)
                val ctxt = #2(exp_w_ctxt2) *)
              in
               exp1 ^
               "    pushq %rax\n" ^
               exp2 ^
               "    popq %rcx\n" ^
               "    and %rcx, %rax\n" 
             end 
              | AST.BOr =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
                (* val e1 = #1(exp_w_ctxt1)
                val e2 = #1(exp_w_ctxt2)
                val ctxt = #2(exp_w_ctxt2) *)
              in
               exp1 ^
               "    pushq %rax\n" ^
               exp2 ^
               "    popq %rcx\n" ^
               "    or %rcx, %rax\n" 
             end 
              | AST.BXor =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
                (* val e1 = #1(exp_w_ctxt1)
                val e2 = #1(exp_w_ctxt2)
                val ctxt = #2(exp_w_ctxt2) *)
              in
              exp1 ^
               "    pushq %rax\n" ^
               exp2 ^
               "    popq %rcx\n" ^
               "    xor %rcx, %rax\n" 
             end 
              | AST.BLeft =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
                (* val e1 = #1(exp_w_ctxt1)
                val e2 = #1(exp_w_ctxt2)
                val ctxt = #2(exp_w_ctxt2) *)
              in
              exp1 ^
               "    pushq %rax\n" ^
               exp2 ^
               "    movq %rax, %rcx\n" ^
               "    popq %rax\n" ^
               "    shlq %cl, %rax\n" 
             end 
              | AST.BRight =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
                (* val e1 = #1(exp_w_ctxt1)
                val e2 = #1(exp_w_ctxt2)
                val ctxt = #2(exp_w_ctxt2) *)
              in
              exp1 ^
               "    pushq %rax\n" ^
               exp2 ^
               "    movq %rax, %rcx\n" ^
               "    popq %rax\n" ^
               "    shrq %cl, %rax\n" 
             end 
              | AST.Mod =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
                (* val e1 = #1(exp_w_ctxt1)
                val e2 = #1(exp_w_ctxt2)
                val ctxt = #2(exp_w_ctxt2) *)
              in
              exp1 ^ "\tpushq %rax\n" ^ exp2 ^
                "\tpopq %rcx\n" ^ "\tpushq %rax\n" ^ "\tmovq %rcx, %rax\n"
                ^ "\tpopq %rcx\n" ^ "\tcqo\n" ^ "\tidiv %rcx\n" ^ 
                "\tmovq %rdx, %rax\n" 
              end
             )
        | AST.Var s =>
          let
            val voffset = VM.find (s, ctxt)
          in
            "    movq " ^ Int.toString voffset ^ "(%rbp), %rax\n"
          end
       | AST.Assign (s, exp) =>
          let
            val expstr = genExp (exp, ctxt)
            val voffset = VM.find (s, ctxt)
          in
            expstr ^
            "    movq %rax, " ^ Int.toString voffset ^ "(%rbp)\n"
          end
     )
  )
  fun genStatement (b : AST.statement * context) : (string * context) =
    (case b
       of (AST.Return exp, ctxt) => 
        (case ctxt
          of (pmap, offset) =>
            let
              val exp = genExp (exp, pmap)
            in
            (exp ^
             "    movq %rbp, %rsp\n" ^
             "    popq %rbp\n" ^
             "    retq\n", ctxt)
           end
        )
      | (AST.Declare (ty, name, opt_exp), ctxt) =>
        let
          val pmap = #1(ctxt)
          val _ = if (VM.contains (name, pmap)) then
                    raise Fail "duplicate declaration"
                  else
                    0
          val offset = #2(ctxt) - 8
        in
          (case opt_exp
            of (SOME exp) => 
              (case ctxt
                of (pmap, offset) =>
                  let
                    val new_exp = genExp (exp, pmap)
                    val new_ctxt = (VM.ins ((name, offset), pmap), offset)
                  in
                    (new_exp ^
                    "\tpushq %rax\n", new_ctxt)
                  end
            )
            | NONE =>
              ("\tpushq $0\n", (VM.ins ((name, offset), pmap), offset))
          )
        end
     | (AST.Exp exp, ctxt) =>
      (case ctxt
        of (pmap, offset) =>
          let
            val new_exp = genExp (exp, pmap)
          in
            (new_exp, ctxt)
          end
      )
    )
  fun genBody (b : (AST.statement list) * context) : string =
    (case b
       of ([], ctxt) => ""
        | ((stm :: rest), ctxt) => 
        let
          val stm_w_context = genStatement (stm, ctxt)
        in
          (case stm_w_context
            of (stm, ctxt2) => stm ^ genBody (rest, ctxt2)
          )
        end
    )
  fun generate (t : AST.func list) : string =
    (case t
       of [] => ""
        | (fnc :: rest) =>
          (case fnc
             of AST.Fun (name, body) =>
                   "\t.globl _" ^ name ^ "\n" ^ 
                   "_" ^ name ^ ":\n" ^
                   "    pushq %rbp\n" ^
                   "    movq %rsp, %rbp\n" ^
                   genBody (body, fresh_context) ^
                   generate rest
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
