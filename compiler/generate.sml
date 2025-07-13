structure Generate = struct
  structure VM = VarMap
  (* pmap maps variables to stack offsets (if types are added it will include
  those as well), and int is the current offset context *)
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
  and genStatement  (b : AST.statement * context) : (string * context) =
    (case b
       of (AST.Return exp, {break_label = bl, continue_label = cl, var_map =
       vmap, current_scope = cs, offset = n}) => 
            let
              val exp = genExp (exp, cs)
              val ctxt = {break_label = bl, continue_label = cl, var_map =
                vmap, current_scope = cs, offset = n}
            in
            (exp ^
             "    movq %rbp, %rsp\n" ^
             "    popq %rbp\n" ^
             "    retq\n",
            ctxt)
           end
     | (AST.Exp exp_option, {break_label = bl, continue_label = cl, var_map =
     vmap, current_scope = cs, offset = n}) =>
         (case exp_option
            of NONE => ("", {break_label = bl, continue_label = cl, var_map =
              vmap, current_scope = cs, offset = n})
             | SOME exp => (genExp (exp, vmap), {break_label = bl,
               continue_label = cl, var_map = vmap, current_scope = cs, offset =
               n})
         )
     | (AST.If (exp, stm1, NONE), {break_label = bl, continue_label = cl,
        var_map = vmap, current_scope = cs, offset = n}) =>
          let
            val curr_context = {break_label = bl, continue_label = cl, var_map =
              vmap, current_scope = cs, offset = n}
            val cond = genExp (exp, cs)
            val post_cond = new_label()
            val (body, ctxt) = genStatement (stm1, curr_context)
          in
           (cond ^
           "    cmpq $0, %rax\n" ^
           "    je " ^ post_cond ^ "\n" ^
           body ^
           post_cond ^ ":\n", curr_context)
          end
    | (AST.If (exp, stm1, SOME stm2), {break_label = bl, continue_label = cl,
       var_map = vmap, current_scope = cs, offset = n}) =>
          let
            val cond = genExp (exp, cs)
            val curr_context ={break_label = bl, continue_label = cl, var_map =
              vmap, current_scope = cs, offset = n} 
            val (if_clause, ctxt) = genStatement (stm1, curr_context)
            val (else_clause, ctxt) = genStatement (stm2, curr_context)
            val else_label = new_label()
            val post_cond = new_label()
          in
           (cond ^
           "    cmpq $0, %rax\n" ^
           "    je " ^ else_label ^ "\n" ^
           if_clause ^
           "    jmp " ^ post_cond ^ "\n" ^
           else_label ^ ":\n" ^
           else_clause ^
           post_cond ^ ":\n", curr_context)
          end
    | (AST.Compound block_items, init_ctxt) =>
      (genBlock (block_items, init_ctxt), init_ctxt)

    | (AST.While (exp, statement), {break_label = bl, continue_label = cl,
       var_map = vmap, current_scope = cs, offset = n}) =>
        let
          val precond_label = new_label()
          val post_label = new_label()
          val control_exp = genExp (exp, cs)
          val (loop_body, ctxt) = genStatement (statement, 
          {break_label = SOME precond_label, continue_label = SOME post_label,
           var_map = vmap, current_scope = cs, offset = n})
        in
          (precond_label ^ ":\n" ^
          control_exp ^
          "    cmpq $0, %rax\n" ^
          "    je " ^ post_label ^ "\n" ^
          loop_body ^
          "    jmp " ^ precond_label ^ "\n" ^
          post_label ^ ":\n"
        , {break_label = bl, continue_label = cl, var_map = vmap,
        current_scope = cs, offset = n})
        end
    | (AST.Do (statement, exp), {break_label = bl, continue_label = cl,
       var_map = vmap, current_scope = cs, offset = n}) =>
        let
          val precond_label = new_label()
          val post_label = new_label()
          val control_exp = genExp (exp, cs)
          val (loop_body, ctxt) = genStatement (statement,{break_label = SOME
          precond_label, continue_label = SOME post_label, var_map = vmap,
          current_scope = cs, offset = n})
        in
          (precond_label ^ ":\n" ^
          loop_body ^
          control_exp ^
          "    cmpq $0, %rax\n" ^
          "    jne " ^ precond_label ^ "\n" ^
          post_label ^ ":\n", {break_label = bl, continue_label = cl,
           var_map = vmap, current_scope = cs, offset = n})
        end
    | (AST.Break, {break_label = bl, continue_label = cl, var_map = vmap,
       current_scope = cs, offset = n}) =>
       (case bl
          of NONE => raise Fail "Break outside of loop body"
           | SOME lbl => ("    jmp " ^ lbl, 
             {break_label = bl, continue_label = cl, var_map = vmap,
              current_scope = cs, offset = n})
       )
    | (AST.Continue, {break_label = bl, continue_label = cl, var_map = vmap,
       current_scope = cs, offset = n}) =>
       (case cl
          of NONE => raise Fail "Continue outside of loop body"
           | SOME lbl => ("    jmp " ^ lbl, 
             {break_label = bl, continue_label = cl, var_map = vmap,
              current_scope = cs, offset = n})
       )
    | (AST.For (exp1, exp2, exp3, body), {break_label = bl, continue_label = cl,
       var_map = vmap, current_scope = cs, offset = n}) =>
       let
         val curr_context = {break_label = bl, continue_label = cl, var_map =
           vmap, current_scope = cs, offset = n}
         val (exp1str, _) = genStatement (AST.Exp exp1, curr_context)
         val exp2str = genExp (exp2, cs)
         val (exp3str, _) = genStatement (AST.Exp exp3, curr_context)
         val cond_label = new_label()
         val end_label = new_label()
         val (loop_body, _) = genStatement (body, {break_label = SOME end_label,
         continue_label = SOME cond_label, var_map = vmap, current_scope = cs,
         offset = n})
       in
         (exp1str ^
         cond_label ^ ":\n" ^
         exp2str ^
         "    cmpq $0, %rax\n" ^
         "    je " ^ end_label ^ "\n" ^
         loop_body ^
         exp3str ^
         "    jmp" ^ end_label ^ "\n" ^
         end_label ^ ":\n", curr_context)
       end
     | (AST.ForDecl (decl, exp2, exp3, body), ctxt) =>
       let
         val (declstr, {break_label = bl, continue_label = cl, var_map = vmap,
         current_scope = cs, offset = n}) = genDeclaration (decl, ctxt)
         val new_ctxt = {break_label = bl, continue_label = cl, var_map = vmap,
         current_scope = cs, offset = n}
         val exp2str = genExp (exp2, cs)
         val (exp3str, _) = genStatement (AST.Exp exp3, new_ctxt)
         val cond_label = new_label()
         val end_label = new_label()
         val (loop_body, _) = genStatement (body, {break_label = SOME end_label,
         continue_label = SOME cond_label, var_map = vmap, current_scope = cs,
         offset = n})
       in
         (declstr ^
         cond_label ^ ":\n" ^
         exp2str ^
         "    cmpq $0, %rax\n" ^
         "    je " ^ end_label ^ "\n" ^
         loop_body ^
         exp3str ^
         "    jmp" ^ end_label ^ "\n" ^
         end_label ^ ":\n", new_ctxt)
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
                    "\tpushq %rax\n", {break_label = bl, continue_label = cl,
                   var_map = new_map, offset = off, current_scope = new_current}
                    )
                  end
            | NONE =>
              let
                val new_map = VM.ins ((name, off), vmap)
                val new_current = VM.ins ((name, off), cs)
              in
                ("\tpushq $0\n", {break_label = bl, continue_label = cl,
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
        genStatement (stm, ctxt)
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
             of AST.Fun (name, body) =>
                   "\t.globl _" ^ name ^ "\n" ^ 
                   "_" ^ name ^ ":\n" ^
                   "    pushq %rbp\n" ^
                   "    movq %rsp, %rbp\n" ^
                   genBlock (body, fresh_context) ^
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
