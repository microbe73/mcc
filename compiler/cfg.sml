structure CFG : sig
  type node
  val convert : AST.prog -> node list
  val rename : AST.func * string * string -> AST.func
  (*val sssa : AST.func -> AST.func (* Handle C variable shadowing *)*)
end = struct
  datatype node
  = BinOp of string * string * AST.bin_operator * string
  | UnOp of string * AST.un_operator * string

  fun convert (_ : AST.prog) = []

  (*AST.func * Set of Variables * Scope level (int) *)
  (*If a variable declared is in the set of variables, prefix it with
   * the scope level. If this is still in the variable set, this must be
   * a variable declared twice in the same block, since C variables cannot start
   * with integers. After this transformation, variable shadowing does not need
   * to be considered during code generation *)
  (*https://www.literateprograms.org/binary_search_tree__standard_ml_.html *)
  (*Sets idk how to do it properly i get its like hash thing but it is what it
     * is*)

  fun rename (fn_w_vars : AST.func * string * string) : AST.func =
  let
    val (func, og, fin) = fn_w_vars
    fun rename_exp (exp : AST.exp) : AST.exp =
      (case exp
         of AST.Const _ => exp
          | AST.UnOp (unop, rhs) => AST.UnOp (unop, rename_exp rhs)
          | AST.BinOp (binop, exp1, exp2) =>
            AST.BinOp (binop, rename_exp exp1, rename_exp exp2)
          | AST.Assign (vname, rhs) =>
            if vname = og then
              AST.Assign (fin, rename_exp rhs)
            else
              AST.Assign (vname, rename_exp rhs)
          | AST.Var name => if name = og then AST.Var fin else AST.Var name
          | AST.Conditional (exp1, exp2, exp3) =>
              AST.Conditional (rename_exp exp1, rename_exp exp2, rename_exp
              exp3)
          | AST.FunCall (fname, exps) =>
              AST.FunCall (fname, List.map rename_exp exps)
      )

    and rename_stm (stm : AST.statement) : AST.statement =
      let
        fun rename_opt_exp (oexp : AST.exp option) = Option.map rename_exp oexp
      in
        (case stm
           of AST.Return exp => AST.Return (rename_exp exp)
            | AST.If (exp1, stm1, NONE) => AST.If (rename_exp exp1, rename_stm
            stm1, NONE)
            | AST.If (exp1, stm1, SOME stm2) => AST.If (rename_exp exp1,
              rename_stm stm1, SOME (rename_stm stm2))
            | AST.Exp NONE => AST.Exp NONE
            | AST.Exp oexp => AST.Exp (rename_opt_exp oexp)
            | AST.For (oexp1, exp2, oexp3, body) =>
                AST.For (rename_opt_exp oexp1, rename_exp exp2, rename_opt_exp
                oexp3, rename_stm body)
            | AST.ForDecl (decl, exp2, oexp, body) =>
                AST.ForDecl (decl, rename_exp exp2, rename_opt_exp oexp,
                rename_stm body)
            | AST.While (exp, body) =>
                AST.While (rename_exp exp, rename_stm body)
            | AST.Do (body, exp) =>
                AST.Do (rename_stm body, rename_exp exp)
            | AST.Break => AST.Break
            | AST.Continue => AST.Continue
            | AST.Compound block => AST.Compound (rename_block block)
        )
      end
    and rename_block (block : AST.block_item list) : AST.block_item list =
      (case block
         of [] => []
          | item :: rest =>
            (case item
              of AST.Statement stm => AST.Statement (rename_stm stm) ::
              rename_block rest
                | AST.Declaration (AST.Declare (ty, name, oexp)) =>
                    if name = og then
                      AST.Declaration (AST.Declare (ty, name, Option.map
                      rename_exp oexp)) :: rest
                (*If there is a redeclaration of this variable, subsequent uses
                   * refer to that redeclaration and should not be renamed*)
                    else
                      AST.Declaration (AST.Declare (ty, name, Option.map
                      rename_exp oexp)) :: rename_block rest
            )
      )
  in
    (case func
       of AST.Fun (_, _, NONE, _) => func
        | AST.Fun (fname, args, SOME body, rettyp) =>
          AST.Fun (fname, args, SOME (rename_block body), rettyp)
    )
  end
end
