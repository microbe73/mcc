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

  
  fun generate (prog : AST.func list) : string =
  let
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
              in
              exp1 ^ "\tpushq %rax\n" ^ exp2 ^
                "\tpopq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
                ^ "\tsetne %al\n" 
             end 
              | AST.Leq =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "\tpushq %rax\n" ^ exp2 ^
                "\tpopq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
                ^ "\tsetle %al\n" 
             end 
              | AST.Geq =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "\tpushq %rax\n" ^ exp2 ^
                "\tpopq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
                ^ "\tsetge %al\n" 
             end 
              | AST.Gt =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "\tpushq %rax\n" ^ exp2 ^
                "\tpopq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
                ^ "\tsetg %al\n" 
             end 
              | AST.Lt =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "\tpushq %rax\n" ^ exp2 ^
                "\tpopq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
                ^ "\tsetl %al\n" 
             end 
              | AST.BAnd =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
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
       | AST.Conditional (e1, e2, e3) =>
        let
          val exp1 = genExp (e1, ctxt)
          val exp2 = genExp (e2, ctxt)
          val exp3 = genExp (e3, ctxt)
          val else_label = new_label()
          val post_cond = new_label()
        in
         exp1 ^
         "    cmpq $0, %rax\n" ^
         "    je " ^ else_label ^ "\n" ^
         exp2 ^
         "    jmp " ^ post_cond ^ "\n" ^
         else_label ^ ":\n" ^
         exp3 ^
         post_cond ^ ":\n"

        end
     )
  )
  and genStatement  (b : AST.statement * VM.pmap * int) : string =
    (case b
       of (AST.Return exp, pmap, offset) => 
            let
              val exp = genExp (exp, pmap)
            in
            exp ^
             "    movq %rbp, %rsp\n" ^
             "    popq %rbp\n" ^
             "    retq\n"
           end
     | (AST.Exp exp, pmap, offset) =>
          genExp (exp, pmap)
     | (AST.If (exp, stm1, NONE), pmap, offset) =>
          let
            val cond = genExp (exp, pmap)
            val post_cond = new_label()
          in
           cond ^
           "    cmpq $0, %rax\n" ^
           "    je " ^ post_cond ^ "\n" ^
           genStatement (stm1, pmap, offset) ^
           post_cond ^ ":\n" 
          end
    | (AST.If (exp, stm1, SOME stm2), pmap, offset) =>
          let
            val cond = genExp (exp, pmap)
            val if_clause = genStatement (stm1, pmap, offset)
            val else_clause = genStatement (stm2, pmap, offset)
            val else_label = new_label()
            val post_cond = new_label()
          in
           cond ^
           "    cmpq $0, %rax\n" ^
           "    je " ^ else_label ^ "\n" ^
           if_clause ^
           "    jmp " ^ post_cond ^ "\n" ^
           else_label ^ ":\n" ^
           else_clause ^
           post_cond ^ ":\n"
          end
    | (AST.Compound block_items, var_map, offset) =>
      genBlock (block_items, var_map, offset, VM.empty_map)
    )
  and sizeOf (t : AST.typ) : int =
    (case t
      of AST.Int => 8
    )
  and genDeclaration (dec_w_ctxt : AST.declaration * VM.pmap * int * VM.pmap) : (string * VM.pmap * int * VM.pmap) =
    (case dec_w_ctxt
      of (AST.Declare (ty, name, opt_exp), var_map, offset, current_scope) =>
        let
          val _ = if (VM.contains (name, current_scope)) then
                    raise Fail "duplicate declaration"
                  else
                    0
          val offset = offset - sizeOf ty
        in
          (case opt_exp
            of (SOME exp) => 
                  let
                    val new_exp = genExp (exp, var_map)
                    val new_map = VM.ins ((name, offset), var_map)
                    val new_current = VM.ins ((name, offset), current_scope)
                  in
                    (new_exp ^
                    "\tpushq %rax\n", 
                    new_map, offset, new_current)
                  end
            | NONE =>
              let
                val new_map = VM.ins ((name, offset), var_map)
                val new_current = VM.ins ((name, offset), current_scope)
              in
                ("\tpushq $0\n", new_map, offset, new_current)
              end
          )
        end
    )
  
  and genBlockItem (b : AST.block_item * VM.pmap * int * VM.pmap) : (string * VM.pmap * int * VM.pmap) =
    (case b
      of (AST.Declaration decl, var_map, offset, current_scope) => 
        genDeclaration (decl, var_map, offset, current_scope)
      | (AST.Statement stm, var_map, offset, current_scope) => 
        (genStatement (stm, var_map, offset), var_map, offset, current_scope)
    )

  and genBlock (b : (AST.block_item list) * VM.pmap * int * VM.pmap) : string =
      (case b
         of ([], var_map, offset, current_scope) => 
               "    addq $" ^ Int.toString (VM.vsize current_scope) ^ ", %rsp\n"
          | ((bitem :: rest), var_map, offset, current_scope) => 
          let
            val bitem_w_context = genBlockItem (bitem, var_map, offset, current_scope)
          in
            (case bitem_w_context
              of (bitem, var_map, offset, current_scope) =>
               bitem ^ genBlock (rest, var_map, offset, current_scope)
            )
          end
      )
  in
    (case prog
       of [] => ""
        | (fnc :: rest) =>
          (case fnc
             of AST.Fun (name, body) =>
                   "\t.globl _" ^ name ^ "\n" ^ 
                   "_" ^ name ^ ":\n" ^
                   "    pushq %rbp\n" ^
                   "    movq %rsp, %rbp\n" ^
                   genBlock (body, VM.empty_map, 0, VM.empty_map) ^
                   generate rest
          )
    )
  end
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
