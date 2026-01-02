structure TypeCheck : sig
  type fnInfo
  type fnStore
  val arg_eq : (AST.typ list) * (AST.typ list) -> bool
  val validate : AST.prog * fnStore -> fnStore
  val empty_store : fnStore
end = struct
  (*Name, parameter types, return type, defined or only declared *)
  type tlist = AST.typ list
  type fnInfo = string * ((string * AST.typ) list) * AST.typ * bool
  type fnStore = fnInfo list
  val empty_store = []

  fun type_exp (exp_w_env : AST.exp * (string * AST.typ list)) =
    AST.Int

  fun arg_eq (args : (AST.typ list) * (AST.typ list)) =
    (case args
      of ([], []) => true
        | (a1 :: rest1, a2 :: rest2) =>
            a1 = a2 andalso arg_eq (rest1, rest2)
        | _ => false
    )
  fun find_fn (name_w_info : string * fnStore) : fnInfo option =
    let
      val (name, infos) = name_w_info
    in
      (case infos
         of [] => NONE
          | info :: rest =>
            let
              val (fn_name, _, _, _) = info
            in
              if fn_name = name then SOME info else
                find_fn (name, rest)
            end
      )
    end
  fun validate (prog_w_decls : AST.prog * fnStore) : fnStore =
    let
      fun validate_exp (exp_w_decls : AST.exp * fnStore) : fnStore =
        let
          val (exp, declared_fns) = exp_w_decls
        in
          (case exp
             of AST.UnOp (_, exp1) => validate_exp (exp1, declared_fns)
              | AST.Conditional (exp1, exp2, exp3) =>
                  let
                    val _ = validate_exp (exp1, declared_fns)
                    val _ = validate_exp (exp2, declared_fns)
                    val _ = validate_exp (exp3, declared_fns)
                  in
                    declared_fns
                  end
              | AST.BinOp (_, exp1, exp2) =>
                  let
                    val _ = validate_exp (exp1, declared_fns)
                    val _ = validate_exp (exp2, declared_fns)
                  in
                    declared_fns
                  end
              | AST.Assign (vname, exp1) => validate_exp (exp1, declared_fns)
              | AST.Const _ => declared_fns
              | AST.Var _ => declared_fns
              | AST.FunCall (name, args) =>
                  (*TODO: Unzip list, get the type of each expression and check*)
                  let
                    val found_info = find_fn (name, declared_fns)
                  in
                    (case found_info
                       of NONE => raise Fail
                       ("Calling undeclared function `" ^ name ^ "`")
                        | SOME (_, arg_types, _, _) =>
                        if length arg_types = length args then declared_fns else
                          raise Fail ("function `" ^ name ^ "`" ^
                          "called with incorrect argument types. Got ")
                    )
                  end
          )
        end
      and validate_statement (stm_w_decls : AST.statement * fnStore) : fnStore =
        let
          val (stm, declared_fns) = stm_w_decls
        in
          (case stm
             of AST.Return exp => validate_exp (exp, declared_fns)
              | AST.Exp (SOME exp) => validate_exp (exp, declared_fns)
              | AST.Exp NONE => declared_fns
              | AST.Compound block_items => validate_block_items (block_items,
                declared_fns)
              | AST.If (exp1, stm1, NONE) =>
                  let
                    val _ = validate_exp (exp1, declared_fns)
                    val _ = validate_statement (stm1, declared_fns)
                  in
                    declared_fns
                  end
              | AST.If (exp1, stm1, SOME stm2) =>
                  let
                    val _ = validate_exp (exp1, declared_fns)
                    val _ = validate_statement (stm1, declared_fns)
                    val _ = validate_statement (stm2, declared_fns)
                  in
                    declared_fns
                  end
              | AST.While (exp1, stm1) =>
                  let
                    val _ = validate_exp (exp1, declared_fns)
                    val _ = validate_statement (stm1, declared_fns)
                  in
                    declared_fns
                  end
              | AST.Do (stm1, exp1) =>
                  let
                    val _ = validate_exp (exp1, declared_fns)
                    val _ = validate_statement (stm1, declared_fns)
                  in
                    declared_fns
                  end
              | AST.Break => declared_fns
              | AST.Continue => declared_fns

          )
        end
      and validate_block_item (blitm_w_decls : AST.block_item * fnStore) :
        fnStore =
        let
          val (item, declared_fns) = blitm_w_decls
        in
          (case item
             of AST.Statement stm => validate_statement (stm, declared_fns)
              | AST.Declaration decl =>
                  (case decl
                     of AST.Declare (ty, vname, SOME exp) => validate_exp (exp,
                     declared_fns)
                      | _ => declared_fns
                  )
          )
        end
      and validate_block_items (items_w_decls : AST.block_item list * fnStore) :
        fnStore =
        (case items_w_decls
           of ([], declared_fns) => declared_fns
            | (item :: rest, declared_fns) =>
                let
                  val _ = validate_block_item (item, declared_fns)
                in
                  validate_block_items (rest, declared_fns)
                end
        )
      val (prog, declared_fns) = prog_w_decls
    in
      (case prog
        of AST.Prog func_decls =>
          (case func_decls
            of (AST.Fun (name, arg_types, NONE, ret_type) :: rest) =>
              validate (AST.Prog rest, (name, arg_types, ret_type, false) ::
              declared_fns)
             | (AST.Fun (name, arg_types, SOME body, ret_type) :: rest) =>
              let
                val new_decls = (name, arg_types, ret_type, false) ::
                declared_fns
                val found_info = find_fn (name, declared_fns)
                (*TODO: Removing the old function from list also probably good, but
                 this works fine for now *)
              in
                (case found_info
                  of NONE =>
                       let
                         val _ = validate_block_items (body, new_decls)
                       in
                         validate (AST.Prog rest, new_decls)
                       end
                    | SOME (_, _, _, false) =>
                       let
                         val _ = validate_block_items (body, new_decls)
                       in
                         validate (AST.Prog rest, new_decls)
                       end
                    | SOME (_, _, _, true) =>
                        raise Fail ("Redefining declared function `" ^ name ^
                        "`")
                )
              end
             | [] => declared_fns
          )
      )
    end

end
  
