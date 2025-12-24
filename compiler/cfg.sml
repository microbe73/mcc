structure CFG : sig
  type node
  val convert : AST.prog -> node list
  val rename : AST.block_item list * string * string -> AST.block_item list
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

  fun rename (fn_w_vars : AST.block_item list * string * string) :
    AST.block_item list =
  let
    val (block, og, fin) = fn_w_vars
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
            | AST.Compound stms => AST.Compound (rename_block stms)
        )
      end
    and rename_block (stms : AST.block_item list) : AST.block_item list =
      (case stms
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
    rename_block block
  end

  fun scoping (func : AST.func) : AST.func =
  (case func
    of AST.Fun (fname, arglist, body, rettype) =>
    let
      fun add_decl (vars_w_newvar : string list * string) : string list option =
        let
          val (vars, newvar) = vars_w_newvar
          fun isvar (v : string) = v = newvar
          val valid = not (List.exists isvar vars)
        in
          if valid then SOME (newvar :: vars) else NONE
        end
      fun scope_stm (stm_w_vars : AST.statement * string list * int) : AST.statement =
        let
          val (stm, vars, depth) = stm_w_vars
        in
          (case stm
             of AST.Return _ => stm
              | AST.Exp _ => stm
              | AST.If (exp1, stm1, NONE) => AST.If (exp1, scope_stm (stm1, vars,
                depth), NONE)

              | AST.If (exp1, stm1, SOME stm2) => AST.If (exp1, scope_stm (stm1,
                vars, depth), SOME (scope_stm (stm2, vars, depth)))

              | AST.For (oexp1, exp2, oexp3, body) =>
                  AST.For (oexp1, exp2, oexp3, scope_stm (body, vars, depth))
              | AST.While (exp, body) =>
                  AST.While (exp, scope_stm (body, vars, depth))

              | AST.Do (body, exp) =>
                  AST.Do (scope_stm (body, vars, depth), exp)

              | AST.Break => AST.Break
              | AST.Continue => AST.Continue

              | AST.ForDecl (AST.Declare (typ, name, oval), exp2, oexp, body) =>
                  let
                    val ovars' = add_decl (vars, name)
                  in
                    (case ovars'
                       of NONE => raise Fail ("Redeclared variable ` " ^
                       name ^ "`in for loop: Function " ^ fname)
                        | SOME vars' =>
                          AST.ForDecl (AST.Declare (typ, name, oval), exp2, oexp,
                          scope_stm (body, vars', depth))
                    )
                  end
              | AST.Compound block => AST.Compound (scope_block (block, vars,
                depth + 1))
          )
        end
      and scope_block (block_w_vars : AST.block_item list * string list * int) : AST.block_item list =
        let
          val (block, vars, depth) = block_w_vars
        in
          (case block
             of [] => []
              | item :: rest =>
                (case item
                  of AST.Statement stm => AST.Statement (scope_stm (stm, vars,
                  depth)) :: scope_block (rest, vars, depth)
                    | AST.Declaration (AST.Declare (ty, name, oexp)) =>
                      let
                        val o_new_vars = add_decl (vars, name)
                      in
                        (case o_new_vars
                          of NONE =>
                              let
                                val dname = Int.toString depth ^ name
                                val o_renewed_vars = add_decl (vars, dname)
                              in
                                (case o_renewed_vars
                                   of NONE => raise Fail
                                   ("Variable `" ^ name ^ "` declared multiple " ^
                                   "times in the same scope: Function " ^ fname)
                                    | SOME renewed_vars =>
                                      let
                                        val renamed_block = rename (rest, name,
                                        dname)
                                        val updated_decl = AST.Declaration
                                        (AST.Declare (ty, dname, oexp))
                                      in
                                        updated_decl :: scope_block
                                        (renamed_block, renewed_vars, depth)
                                      end
                                )
                              end
                            | SOME newvars =>
                              item :: scope_block (rest, newvars, depth)
                        )
                      end
                )
          )
        end
      in
        (case body
           of NONE => func
            | SOME block =>
              let
                val argnames = map #1 arglist
                val scoped_argnames = map (fn s => "0" ^ s) argnames
                val initial_scope = argnames @ scoped_argnames
                val updated_body = scope_block (block, initial_scope, 0)
              in
                AST.Fun (fname, arglist, SOME updated_body, rettype)
              end
        )
      end
    )
end
