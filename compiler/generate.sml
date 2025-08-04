structure Generate = struct
  structure VM = VarMap

  type context = {var_map : VM.pmap, offset : int, continue_label : string
  option, break_label : string option, current_scope : VM.pmap }

  val fresh_context : context =
    {var_map=VM.empty_map, offset = 0, continue_label = NONE, break_label =
    NONE, current_scope = VM.empty_map }




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
         of AST.Const n => "    movq $" ^ Int.toString n ^ ", %rax\n"
          | AST.UnOp (unop, inner_exp) =>
              (case unop
                 of AST.Negation =>
                    (case genExp (inner_exp, ctxt)
                      of new_exp  =>
                        new_exp ^ "    neg  %rax\n"
                    )
                  | AST.Complement =>
                    (case genExp (inner_exp, ctxt)
                      of new_exp  =>
                        new_exp ^ "    not  %rax\n"
                    )
                  | AST.Not =>
                    (case genExp (inner_exp, ctxt)
                      of new_exp  =>
                        new_exp ^ "    cmpq  $0, %rax\n" ^
                      "    movq  $0, %rax\n" ^ "\tsete  %al\n"
                    )
               )
          | AST.BinOp (binop, e1, e2) =>
            (case binop
              of AST.Plus =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
               exp1 ^ "    pushq %rax\n" ^ exp2 ^
              "    popq %rcx\n" ^ "\tadd %rcx, %rax\n"
              end
              | AST.Minus =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "    pushq %rax\n" ^ exp2 ^
              "    popq %rcx\n" ^ "\tsub %rax, %rcx\n" ^ "\tmovq %rcx, %rax\n"
              end
              | AST.Times =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "    pushq %rax\n" ^ exp2 ^
              "    popq %rcx\n" ^ "\timul %rcx, %rax\n"
             end
              | AST.Div =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "    pushq %rax\n" ^ exp2 ^
                "    popq %rcx\n" ^ "\tpushq %rax\n" ^ "\tmovq %rcx, %rax\n"
                ^ "    popq %rcx\n" ^ "\tcqo\n" ^ "\tidiv %rcx\n"
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
              exp1 ^ "    pushq %rax\n" ^ exp2 ^
                "    popq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
                ^ "    sete %al\n"
             end
              | AST.Neq =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "    pushq %rax\n" ^ exp2 ^
                "    popq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
                ^ "    setne %al\n"
             end
              | AST.Leq =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "    pushq %rax\n" ^ exp2 ^
                "    popq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
                ^ "    setle %al\n"
             end
              | AST.Geq =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "    pushq %rax\n" ^ exp2 ^
                "    popq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
                ^ "    setge %al\n"
             end
              | AST.Gt =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "    pushq %rax\n" ^ exp2 ^
                "    popq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
                ^ "    setg %al\n"
             end
              | AST.Lt =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "    pushq %rax\n" ^ exp2 ^
                "    popq %rcx\n" ^ "\tcmpq %rax, %rcx\n" ^ "\tmovq $0, %rax\n"
                ^ "    setl %al\n"
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
              exp1 ^ "    pushq %rax\n" ^ exp2 ^
                "    popq %rcx\n" ^ "\tpushq %rax\n" ^ "\tmovq %rcx, %rax\n"
                ^ "    popq %rcx\n" ^ "\tcqo\n" ^ "\tidiv %rcx\n" ^
                "    movq %rdx, %rax\n"
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
        | AST.FunCall (name, arglist) =>
            genFunCall (rev arglist, ctxt) ^ "\n" ^
            "    call _" ^ name ^ "\n" ^
            "    addq $" ^ Int.toString (length arglist) ^ ", %rsp\n"

     )
  )
  and genFunCall (args_w_ctxt : AST.exp list * VM.pmap) : string =
    (case args_w_ctxt
       of ([], ctxt) => ""
        | ((arg :: rest), ctxt) =>
            let
              val exp_str = genExp (arg, ctxt)
            in
              exp_str ^ "\n" ^
              "    pushq %rax\n"
            end
    )
  and genStatement  (b : AST.statement * context) : string =
    (case b
       of (AST.Return exp, {break_label = bl, continue_label = cl, var_map =
       vmap, current_scope = cs, offset = n}) =>
            let
              val exp = genExp (exp, cs)
              val ctxt = {break_label = bl, continue_label = cl, var_map =
                vmap, current_scope = cs, offset = n}
            in
            exp ^
             "    movq %rbp, %rsp\n" ^
             "    popq %rbp\n" ^
             "    retq\n"
           end
     | (AST.Exp exp_option, {break_label = bl, continue_label = cl, var_map =
     vmap, current_scope = cs, offset = n}) =>
         (case exp_option
            of NONE => ""
             | SOME exp => genExp (exp, vmap)
         )
     | (AST.If (exp, stm1, NONE), {break_label = bl, continue_label = cl,
        var_map = vmap, current_scope = cs, offset = n}) =>
          let
            val curr_context = {break_label = bl, continue_label = cl, var_map =
              vmap, current_scope = cs, offset = n}
            val cond = genExp (exp, cs)
            val post_cond = new_label()
            val body = genStatement (stm1, curr_context)
          in
           cond ^
           "    cmpq $0, %rax\n" ^
           "    je " ^ post_cond ^ "\n" ^
           body ^
           post_cond ^ ":\n"
          end
    | (AST.If (exp, stm1, SOME stm2), {break_label = bl, continue_label = cl,
       var_map = vmap, current_scope = cs, offset = n}) =>
          let
            val cond = genExp (exp, cs)
            val curr_context ={break_label = bl, continue_label = cl, var_map =
              vmap, current_scope = cs, offset = n}
            val if_clause = genStatement (stm1, curr_context)
            val else_clause = genStatement (stm2, curr_context)
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
    | (AST.Compound block_items, init_ctxt) =>
      genBlock (block_items, init_ctxt)

    | (AST.While (exp, statement), {break_label = bl, continue_label = cl,
       var_map = vmap, current_scope = cs, offset = n}) =>
        let
          val precond_label = new_label()
          val post_label = new_label()
          val control_exp = genExp (exp, cs)
          val loop_body = genStatement (statement,
          {break_label = SOME precond_label, continue_label = SOME post_label,
           var_map = vmap, current_scope = cs, offset = n})
        in
          precond_label ^ ":\n" ^
          control_exp ^
          "    cmpq $0, %rax\n" ^
          "    je " ^ post_label ^ "\n" ^
          loop_body ^
          "    jmp " ^ precond_label ^ "\n" ^
          post_label ^ ":\n"
        end
    | (AST.Do (statement, exp), {break_label = bl, continue_label = cl,
       var_map = vmap, current_scope = cs, offset = n}) =>
        let
          val precond_label = new_label()
          val post_label = new_label()
          val control_exp = genExp (exp, cs)
          val loop_body = genStatement (statement,{break_label = SOME
          precond_label, continue_label = SOME post_label, var_map = vmap,
          current_scope = cs, offset = n})
        in
          precond_label ^ ":\n" ^
          loop_body ^
          control_exp ^
          "    cmpq $0, %rax\n" ^
          "    jne " ^ precond_label ^ "\n" ^
          post_label ^ ":\n"
        end
    | (AST.Break, {break_label = bl, continue_label = cl, var_map = vmap,
       current_scope = cs, offset = n}) =>
       (case bl
          of NONE => raise Fail "Break outside of loop body"
           | SOME lbl => "    jmp " ^ lbl
       )
    | (AST.Continue, {break_label = bl, continue_label = cl, var_map = vmap,
       current_scope = cs, offset = n}) =>
       (case cl
          of NONE => raise Fail "Continue outside of loop body"
           | SOME lbl => "    jmp " ^ lbl
       )
    | (AST.For (exp1, exp2, exp3, body), {break_label = bl, continue_label = cl,
       var_map = vmap, current_scope = cs, offset = n}) =>
       let
         val curr_context = {break_label = bl, continue_label = cl, var_map =
           vmap, current_scope = cs, offset = n}
         val exp1str = genStatement (AST.Exp exp1, curr_context)
         val exp2str = genExp (exp2, cs)
         val exp3str = genStatement (AST.Exp exp3, curr_context)
         val cond_label = new_label()
         val end_label = new_label()
         val loop_body = genStatement (body, {break_label = SOME end_label,
         continue_label = SOME cond_label, var_map = vmap, current_scope = cs,
         offset = n})
       in
         exp1str ^
         cond_label ^ ":\n" ^
         exp2str ^
         "    cmpq $0, %rax\n" ^
         "    je " ^ end_label ^ "\n" ^
         loop_body ^
         exp3str ^
         "    jmp" ^ end_label ^ "\n" ^
         end_label ^ ":\n"
       end
     | (AST.ForDecl (decl, exp2, exp3, body), ctxt) =>
       let
         val (declstr, {break_label = bl, continue_label = cl, var_map = vmap,
         current_scope = cs, offset = n}) = genDeclaration (decl, ctxt)
         val new_ctxt = {break_label = bl, continue_label = cl, var_map = vmap,
         current_scope = cs, offset = n}
         val exp2str = genExp (exp2, cs)
         val exp3str  = genStatement (AST.Exp exp3, new_ctxt)
         val cond_label = new_label()
         val end_label = new_label()
         val loop_body = genStatement (body, {break_label = SOME end_label,
         continue_label = SOME cond_label, var_map = vmap, current_scope = cs,
         offset = n})
       in
         declstr ^
         cond_label ^ ":\n" ^
         exp2str ^
         "    cmpq $0, %rax\n" ^
         "    je " ^ end_label ^ "\n" ^
         loop_body ^
         exp3str ^
         "    jmp" ^ end_label ^ "\n" ^
         end_label ^ ":\n"
       end
    )

  and sizeOf (t : AST.typ) : int =
    (case t
      of AST.Int => 8
    )

  and genDeclaration (dec_w_ctxt : AST.declaration * context) : (string *
    context) =
    (case dec_w_ctxt
      of (AST.Declare (ty, name, opt_exp), {break_label = bl, continue_label =
      cl, var_map = vmap, current_scope = cs, offset = n}) =>
        let
          val _ = if (VM.contains (name, cs)) then
                    raise Fail "duplicate declaration"
                  else
                    0
          val off = n - sizeOf ty
        in
          (case opt_exp
            of (SOME exp) =>
                  let
                    val new_exp = genExp (exp, vmap)
                    val new_map = VM.ins ((name, off), vmap)
                    val new_current = VM.ins ((name, off), cs)
                  in
                    (new_exp ^
                    "    pushq %rax\n", {break_label = bl, continue_label = cl,
                   var_map = new_map, offset = off, current_scope = new_current}
                    )
                  end
            | NONE =>
              let
                val new_map = VM.ins ((name, off), vmap)
                val new_current = VM.ins ((name, off), cs)
              in
                ("    pushq $0\n", {break_label = bl, continue_label = cl,
                   var_map = new_map, offset = off, current_scope = new_current})
              end
          )
        end
    )

  and genBlockItem (b : AST.block_item * context) : (string * context) =
    (case b
      of (AST.Declaration decl, ctxt) =>
        genDeclaration (decl, ctxt)
      | (AST.Statement stm, ctxt) =>
        (genStatement (stm, ctxt), ctxt)
    )

  and genBlock (b : (AST.block_item list) * context) : string =
      (case b
         of ([], {break_label = bl, continue_label = cl,
           var_map = vmap, current_scope = cs, offset = n}) =>
               "    addq $" ^ Int.toString (VM.vsize cs) ^ ", %rsp\n"
          | ((bitem :: rest), init_ctxt) =>
          let
            val (bitem, new_ctxt) = genBlockItem (bitem, init_ctxt)
          in
            bitem ^ genBlock (rest, new_ctxt)
          end
      )
  in
    (case prog
       of [] => ""
        | (fnc :: rest) =>
          (case fnc
             of AST.Fun (name, arglist, SOME body, ret_type)  =>
                   "    .globl _" ^ name ^ "\n" ^
                   "_" ^ name ^ ":\n" ^
                   "    pushq %rbp\n" ^
                   "    movq %rsp, %rbp\n" ^
                   genBlock (body, fresh_context) ^
                   generate rest
              | AST.Fun (name, arglist, NONE, ret_type) =>
                  ""
          )
    )
  end

end
