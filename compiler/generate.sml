structure Generate = struct
  structure VM = VarMap

  type context = {var_map : VM.pmap, offset : int, continue_label : string
  option, break_label : string option, current_scope : VM.pmap }

  val fresh_context : context =
    {var_map=VM.empty_map, offset = 0, continue_label = NONE, break_label =
    NONE, current_scope = VM.empty_map }




  val count = ref 0
  fun incr (_ : unit) : int =
    let val _ = count := !count + 1 in !count end

  fun new_label (a : unit) : string =
  let
    val _ = count := !count + 1
  in
    "_label" ^ Int.toString (!count)
  end

  fun stack_offset (ctxt : VM.pmap * int) : int =
    (case ctxt
      of (vmap, n) =>
      (case vmap
        of ((name, VM.Register s):: rest) => stack_offset (rest, n)
         | ((name, VM.Offset m) :: rest) =>
         if m < n then stack_offset (rest, m) else stack_offset (rest, n)
         | _ => n
      )
    )
  fun generate (prog : AST.func list) : string =
  let
  fun genExp (exp_w_context : AST.exp * VM.pmap) : string =
    (case exp_w_context of (exp, ctxt) =>
    let
      val off = stack_offset (ctxt, 0)
    in
      (case exp
         of AST.Const n => "    movl $" ^ Int.toString n ^ ", %eax\n"
          | AST.UnOp (unop, inner_exp) =>
              (case unop
                 of AST.Negation =>
                    genExp (inner_exp, ctxt)
                    ^ "    neg  %eax\n"
                  | AST.Complement =>
                    genExp (inner_exp, ctxt)
                    ^ "    not  %eax\n"
                  | AST.Not =>
                    genExp (inner_exp, ctxt)
                    ^ "    cmpl  $0, %eax\n" ^
                      "    movl  $0, %eax\n" ^ 
                      "    sete  %al\n"
               )
          | AST.BinOp (binop, e1, e2) =>
            (case binop
              of AST.Plus =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
               exp1 ^
               "    push %eax\n" ^
               exp2 ^
              "    pop %ecx\n" ^
              "    add %rcx, %rax\n"
              end
              | AST.Minus =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^
              "    push %eax\n" ^
              exp2 ^
              "    pop %ecx\n" ^
              "    sub %eax, %ecx\n" ^
              "    movl %ecx, %eax\n"
              end
              | AST.Times =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^
              "    push %eax\n" ^
              exp2 ^
              "    pop %ecx\n" ^
              "    imulq %rcx, %rax\n"
             end
              | AST.Div =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^
              "    push %eax\n" ^
              exp2 ^
              "    pop %ecx\n" ^
              "    push %eax\n" ^
              "    movl %ecx, %eax\n" ^
              "    pop %ecx\n" ^
              "    cqo\n" ^
              "    idiv %ecx\n"
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
                 "    cmpl $0, %eax\n" ^
                 "    je " ^ clause2 ^ "\n" ^
                 "    movl $1, %eax\n" ^
                 "    jmp " ^ end_label ^ "\n" ^
                 clause2 ^ ":\n" ^
                 exp2 ^
                 "    cmpl $0, %eax\n" ^
                 "    movl $0, %eax\n" ^
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
                 "    cmpl $0, %eax\n" ^
                 "    jne " ^ clause2 ^ "\n" ^
                 "    jmp " ^ end_label ^ "\n" ^
                 clause2 ^ ":\n" ^
                 exp2 ^
                 "    cmpl $0, %eax\n" ^
                 "    movl $0, %eax\n" ^
                 "    setne %al\n" ^
                 end_label ^ ":\n"
                end
             end
              | AST.Eq =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "    push %eax\n" ^ exp2 ^
                "    pop %ecx\n" ^ "    cmpl %eax, %ecx\n" ^ "    movl $0, %eax\n"
                ^ "    sete %al\n"
             end
              | AST.Neq =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "    push %eax\n" ^ exp2 ^
                "    pop %ecx\n" ^ "    cmpl %eax, %ecx\n" ^ "    movl $0, %eax\n"
                ^ "    setne %al\n"
             end
              | AST.Leq =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "    push %eax\n" ^ exp2 ^
                "    pop %ecx\n" ^ "    cmpl %eax, %ecx\n" ^ "    movl $0, %eax\n"
                ^ "    setle %al\n"
             end
              | AST.Geq =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "    push %eax\n" ^ exp2 ^
                "    pop %ecx\n" ^ "    cmpl %eax, %ecx\n" ^ "    movl $0, %eax\n"
                ^ "    setge %al\n"
             end
              | AST.Gt =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "    push %eax\n" ^ exp2 ^
                "    pop %ecx\n" ^ "    cmpl %eax, %ecx\n" ^ "    movl $0, %eax\n"
                ^ "    setg %al\n"
             end
              | AST.Lt =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^ "    push %eax\n" ^ exp2 ^
                "    pop %ecx\n" ^ "    cmpl %eax, %ecx\n" ^ "    movl $0, %eax\n"
                ^ "    setl %al\n"
             end
              | AST.BAnd =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
               exp1 ^
               "    push %eax\n" ^
               exp2 ^
               "    pop %ecx\n" ^
               "    and %ecx, %eax\n"
             end
              | AST.BOr =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
               exp1 ^
               "    push %eax\n" ^
               exp2 ^
               "    pop %ecx\n" ^
               "    or %ecx, %eax\n"
             end
              | AST.BXor =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^
               "    push %eax\n" ^
               exp2 ^
               "    pop %ecx\n" ^
               "    xor %ecx, %eax\n"
             end
              | AST.BLeft =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^
               "    push %eax\n" ^
               exp2 ^
               "    movl %eax, %ecx\n" ^
               "    pop %eax\n" ^
               "    shlq %cl, %rax\n"
             end
              | AST.BRight =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
              exp1 ^
               "    push %eax\n" ^
               exp2 ^
               "    movl %eax, %ecx\n" ^
               "    pop %eax\n" ^
               "    shrq %cl, %rax\n"
             end
              | AST.Mod =>
              let
                val exp1 = genExp (e1, ctxt)
                val exp2 = genExp (e2, ctxt)
              in
                exp1 ^
                "    push %eax\n" ^
                exp2 ^
                "    pop %ecx\n" ^
                "    push %eax\n" ^
                "    movl %ecx, %eax\n" ^
                "    pop %ecx\n" ^
                "    cqo\n" ^
                "    idiv %rcx\n" ^
                "    movl %edx, %eax\n"
              end
             )
        | AST.Var s =>
          let
            val voffset = VM.find (s, ctxt)
          in
            (case voffset
               of VM.Offset n =>
                "    movl " ^ Int.toString n ^ "(%rbp), %eax\n"
                | VM.Register reg =>
                "    movl " ^ reg ^ ", %eax\n"
            )
          end
       | AST.Assign (s, new_exp) =>
          let
            val expstr = genExp (new_exp, ctxt)
            val voffset = VM.find (s, ctxt)
          in
            (case voffset
               of VM.Offset n =>
                expstr ^
                "    movl %eax, " ^ Int.toString n ^ "(%rbp)\n"
                | VM.Register reg =>
                expstr ^
                "    movl %eax, " ^ reg ^ ", %rax\n"
            )
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
            let
              val bytes_to_add = if length arglist > 6 
                                 then
                                   8 * ((length arglist) - 6)
                                 else
                                   0
            in
              "    movq %rsp, %rax\n" ^
              "    subq $" ^ Int.toString (8 * (bytes_to_add + 1)) ^ ", %rax\n" ^
              "    xorq %rdx, %rdx\n" ^
              "    movq $0x20, %rcx\n" ^
              "    idivq %rcx\n" ^
              "    subq %rdx, %rsp\n" ^
              "    pushq %rdx\n" ^
              genFunCall (arglist, ctxt, 1) ^
              "    popq %rdx\n" ^
              "    addq %rdx, %rsp\n" ^
              "    call _" ^ name ^ "\n" ^
              "    addq $" ^ Int.toString bytes_to_add ^ ", %rsp\n"
            end
     )
     end
  )
  and genFunCall (args_w_ctxt : AST.exp list * VM.pmap * int) : string =
    (case args_w_ctxt
       of ([], _, _) => ""
        | ((arg :: rest), ctxt, n) =>
            let
              val exp_str = genExp (arg, ctxt)
            in
              if n < 6 then
                exp_str ^
                "    movl %eax," ^ nextReg n ^ "\n" ^
                genFunCall (rest, ctxt, n + 1)
              else if n = 6 then
                exp_str ^
                "    movl %eax," ^ nextReg n ^ "\n" ^
                genFunCall (rev rest, ctxt, n + 1)
              else
                exp_str ^
                "    push %eax\n" ^
                genFunCall (rest, ctxt, n + 1)
            end
    )
  and nextReg (n : int) : string =
    (case n
       of 1 => "%edi"
        | 2 => "%esi"
        | 3 => "%edx"
        | 4 => "%ecx"
        | 5 => "%e8"
        | 6 => "%e9"
        | _ => raise Fail "out of registers"
    )
  and genStatement  (b : AST.statement * context) : string =
    (case b
       of (AST.Return exp, {break_label = _, continue_label = _, var_map = vmap,
       current_scope = _, offset = _}) =>
            let
              val exp = genExp (exp, vmap)
            in
            exp ^
             "    movq %rbp, %rsp\n" ^
             "    popq %rbp\n" ^
             "    ret\n"
           end
     | (AST.Exp exp_option, {break_label = _, continue_label = _, var_map =
     vmap, current_scope = _, offset = _}) =>
         (case exp_option
            of NONE => ""
             | SOME exp => genExp (exp, vmap)
         )
     | (AST.If (exp, stm1, NONE), {break_label = bl, continue_label = cl,
        var_map = vmap, current_scope = cs, offset = n}) =>
          let
            val curr_context = {break_label = bl, continue_label = cl, var_map =
              vmap, current_scope = cs, offset = n}
            val cond = genExp (exp, vmap)
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
            val cond = genExp (exp, vmap)
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
    | (AST.Compound block_items, {break_label = bl, continue_label = cl,
       var_map = vmap, current_scope = cs, offset = n}) =>
      genBlock (block_items, {break_label = bl, continue_label = cl,
       var_map = vmap, current_scope = VM.empty_map, offset = n})

    | (AST.While (exp, statement), {break_label = _, continue_label = _,
       var_map = vmap, current_scope = cs, offset = n}) =>
        let
          val precond_label = new_label()
          val post_label = new_label()
          val control_exp = genExp (exp, vmap)
          val loop_body = genStatement (statement,
          {break_label = SOME post_label, continue_label = SOME precond_label,
           var_map = vmap, current_scope = VM.empty_map, offset = n})
        in
          precond_label ^ ":\n" ^
          control_exp ^
          "    cmpq $0, %rax\n" ^
          "    je " ^ post_label ^ "\n" ^
          loop_body ^
          "    jmp " ^ precond_label ^ "\n" ^
          post_label ^ ":\n"
        end
    | (AST.Do (statement, exp), {break_label = _, continue_label = _,
       var_map = vmap, current_scope = cs, offset = n}) =>
        let
          val precond_label = new_label()
          val post_label = new_label()
          val control_exp = genExp (exp, vmap)
          val loop_body = genStatement (statement,{break_label = SOME
          post_label, continue_label = SOME precond_label, var_map = vmap,
          current_scope = VM.empty_map, offset = n})
        in
          precond_label ^ ":\n" ^
          loop_body ^
          control_exp ^
          "    cmpq $0, %rax\n" ^
          "    jne " ^ precond_label ^ "\n" ^
          post_label ^ ":\n"
        end
    | (AST.Break, {break_label = bl, continue_label = _, var_map = _,
       current_scope = _, offset = _}) =>
       (case bl
          of NONE => raise Fail "Break outside of loop body"
           | SOME lbl => "    jmp " ^ lbl ^ "\n"
       )
    | (AST.Continue, {break_label = _, continue_label = cl, var_map = _,
       current_scope = _, offset = _}) =>
       (case cl
          of NONE => raise Fail "Continue outside of loop body"
           | SOME lbl => "    jmp " ^ lbl ^ "\n"
       )
    | (AST.For (exp1, exp2, exp3, body), {break_label = bl, continue_label = cl,
       var_map = vmap, current_scope = cs, offset = n}) =>
       let
         val header_context = {break_label = bl, continue_label = cl, var_map =
           vmap, current_scope = VM.empty_map, offset = n}
         val exp1str = genStatement (AST.Exp exp1, header_context)
         val exp2str = genExp (exp2, vmap)
         val exp3str = genStatement (AST.Exp exp3, header_context)
         val cond_label = new_label()
         val end_label = new_label()
         val loop_body = genStatement (body, {break_label = SOME end_label,
         continue_label = SOME cond_label, var_map = vmap, current_scope =
         VM.empty_map,
         offset = n})
       in
         exp1str ^
         cond_label ^ ":\n" ^
         exp2str ^
         "    cmpl $0, %rax\n" ^
         "    je " ^ end_label ^ "\n" ^
         loop_body ^
         exp3str ^
         "    jmp " ^ end_label ^ "\n" ^
         end_label ^ ":\n"
       end
     | (AST.ForDecl (decl, exp2, exp3, body), ctxt) =>
       let
         val (declstr, {break_label = bl, continue_label = cl, var_map = vmap,
         current_scope = cs, offset = n}) = genDeclaration (decl, ctxt)
         val new_ctxt = {break_label = bl, continue_label = cl, var_map = vmap,
         current_scope = cs, offset = n}
         val exp2str = genExp (exp2, vmap)
         val exp3str  = genStatement (AST.Exp exp3, new_ctxt)
         val cond_label = new_label()
         val end_label = new_label()
         val loop_body = genStatement (body, {break_label = SOME end_label,
         continue_label = SOME cond_label, var_map = vmap, current_scope =
         VM.empty_map, offset = n})
       in
         declstr ^
         cond_label ^ ":\n" ^
         exp2str ^
         "    cmpq $0, %rax\n" ^
         "    je " ^ end_label ^ "\n" ^
         loop_body ^
         exp3str ^
         "    jmp " ^ end_label ^ "\n" ^
         end_label ^ ":\n"
       end
    )

  and sizeOf (t : AST.typ) : int =
    (case t
      of AST.Int => 4
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
                    val new_map = VM.ins ((name, VM.Offset off), vmap)
                    val new_current = VM.ins ((name, VM.Offset off), cs)
                  in
                    (new_exp ^
                    "    pushq %rax\n", {break_label = bl, continue_label = cl,
                   var_map = new_map, offset = off, current_scope = new_current}
                    )
                  end
            | NONE =>
              let
                val new_map = VM.ins ((name, VM.Offset off), vmap)
                val new_current = VM.ins ((name, VM.Offset off), cs)
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
         of ([], {break_label = _, continue_label = _,
           var_map = _, current_scope = cs, offset = _}) =>
               "    addq $" ^ Int.toString (VM.vsize cs) ^ ", %rsp\n"
          | ((bitem :: rest), init_ctxt) =>
          let
            val (bitem, new_ctxt) = genBlockItem (bitem, init_ctxt)
          in
            bitem ^ genBlock (rest, new_ctxt)
          end
      )
  and genFunMap (args_w_count : (string * AST.typ) list * int) : VM.pmap  =
    (case args_w_count
       of ([], _) => []
        | ((name, typ) :: rest, n) =>
          (name, VM.Offset (~1 * (sizeOf typ) * n)) :: genFunMap (rest, n + 1)
    )
  and genArgStack (arglist : (string * AST.typ) list * int) : string =
    (case arglist
       of ([], _) => ""
        | ((_, _) :: rest, n) =>
            "    pushq " ^ nextReg n ^ "\n" ^
            genArgStack (rest, n + 1)
    )
  in
    (case prog
       of [] => ""
        | (fnc :: rest) =>
          (case fnc
             of AST.Fun (name, arglist, SOME body, _)  =>
                  let
                    val base_map = genFunMap (arglist, 1)
                    val n = length arglist
                    val (arg_name, arg_types) = ListPair.unzip arglist
                    val adder = fn (x : int * int) => #1(x) + #2(x)
                    val off = foldl adder 0 (map sizeOf arg_types)
                    val base_context = {break_label=NONE, continue_label=NONE,
                    var_map=base_map, offset=off, current_scope=base_map}
                  in
                     "\n    .globl _" ^ name ^ "\n" ^
                     "_" ^ name ^ ":\n" ^
                     "    pushq %rbp\n" ^
                     "    movq %rsp, %rbp\n" ^
                     genArgStack (arglist, 1) ^
                     genBlock (body, base_context) ^
                     "    movq %rbp, %rsp\n" ^
                     "    popq %rbp\n" ^
                     "    ret" ^
                     generate rest
                    end
              | AST.Fun (name, _, NONE, _) =>
                  "    .globl _" ^ name ^ "\n" ^ generate rest
          )
    )
  end

end
