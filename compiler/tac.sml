structure CFG : sig
  type node
  val convert : AST.prog -> node list
  (*val sssa : AST.func -> AST.func (* Handle C variable shadowing *)*)
end = struct
  datatype node
  = BinOp of string * string * AST.bin_operator * string
  | UnOp of string * AST.un_operator * string

  fun convert (ast : AST.prog) = []

  (*AST.func * Set of Variables * Scope level (int) *)
  (*If a variable declared is in the set of variables, prefix it with
   * the scope level. If this is still in the variable set, this must be
   * a variable declared twice in the same block, since C variables cannot start
   * with integers. After this transformation, variable shadowing does not need
   * to be considered during code generation *)
  (*fun induct (func : AST.func) : unit =*)
  (*  (case func*)
  (*     of AST.Fun (name, args, NONE, retval) => ()*)
  (*       | AST.Fun (name, args, SOME body, retval) =>*)
  (*      (case body*)
  (*        of [] => ()*)
  (*          | block_item :: rest =>*)
  (*          (case block_item*)
  (*            of AST.Statement stm => ()*)
  (*              | AST.Declaration (AST.Declare (ty, name, NONE)) => ()*)
  (*              | AST.Declaration (AST.Declare (ty, name, SOME exp)) =>*)
  (*                (case exp*)
  (*                   of AST.Const n => ()*)
  (*                    | AST.UnOp (unop, exp) => () (* AST.UnOp (unop, induct exp) *)*)
  (*                )*)
  (*          )*)
  (*      )*)
  (*  )*)
  fun rename (fn_w_vars : AST.func * string * string) : AST.func =
  let
    val (func, og_name, final_name) = fn_w_vars
    fun rename_exp (exp_w_names : AST.exp * string * string) =
      let
        val (exp, og, fin) = exp_w_names
      in
        (case exp
           of AST.Const n => exp
            | AST.UnOp (unop, exp) => AST.UnOp (unop, rename_exp (exp, og,
              fin))
            | AST.BinOp (binop, exp1, exp2) =>
              AST.BinOp (binop, rename_exp (exp1, og, fin), rename_exp (exp2,
              og, fin))
            | AST.Assign (vname, exp) =>
              if vname = og then
                AST.Assign (fin, rename_exp (exp, og, fin))
              else
                AST.Assign (vname, rename_exp (exp, og, fin))
            | AST.Var name => if name = og then AST.Var fin else AST.Var name
            | AST.Conditional (exp1, exp2, exp3) =>
                AST.Conditional (rename_exp (exp1, og, fin), rename_exp (exp2,
                og, fin), rename_exp (exp3, og, fin))
            | AST.FunCall (fname, exps) =>
                AST.FunCall (fname, List.map (fn ex => rename_exp (ex, og, fin))
                exps)
        )
      end
  in
    func
  end
    
end
