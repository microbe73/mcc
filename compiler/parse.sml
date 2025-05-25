structure Parse = struct

  structure T = Token
  type toklist = Token.token list
  type binlist = AST.bin_operator list
  fun progAST (tlist : Token.token list) : AST.func list =
    let
      fun nextFun (tlist : Token.token list) : (AST.func * (Token.token list))
        =
        (case tlist
           of T.KW (T.Int) :: T.Identifier fname :: T.OPar :: T.CPar :: T.OBrac ::
           rest =>
                   let
                     val next = nextBlockItemList rest
                   in
                     (case next
                        of (statements, toks) => 
                        (AST.Fun (fname, statements), toks)
                     )
                   end
            | _ => raise Fail "Parse error, unable to find function declaration"

        )
      and nextGenExp (ops_w_lvl : AST.exp * toklist * (toklist -> AST.exp * toklist) * AST.bin_operator *
      (AST.exp * toklist -> AST.exp * toklist)) : AST.exp * toklist =
        (case ops_w_lvl
          of (term, toks, nextPrecFn, binop, caller) =>
            let
              val next_term = nextPrecFn toks
            in
              (case next_term
                of (nterm, ntoks) =>
                  caller ((AST.BinOp (binop, term, nterm)), ntoks)
              )
            end
        )
      and nextExp (tlist : toklist) : AST.exp * toklist =
        (case tlist
          of T.Identifier s :: T.Asgn :: rest =>
            let
              val next_w_toks = nextExp rest
            in
              (case next_w_toks
                of (next, toks) => (AST.Assign (s, next), toks)
              )
            end
           | _ => nextLOExp tlist
        )
(*Assignment/Assignment operators like +=*)
      and nextLOExpHelper (term_w_tlist : AST.exp * toklist) :
        AST.exp * (toklist) =
        (case term_w_tlist
          of (term, T.OR :: rest) =>
            nextGenExp (term, rest, nextLAExp, AST.OR, nextLOExpHelper)
           | _ => term_w_tlist
        )

      and nextLOExp (tlist : toklist) :
        AST.exp * toklist =
        nextLOExpHelper (nextLAExp (tlist))

      and nextLAExpHelper (term_w_tlist : AST.exp * toklist) :
        AST.exp * (toklist) =
        (case term_w_tlist
          of (term, T.AND :: rest) =>
            nextGenExp (term, rest, nextBORExp, AST.AND, nextLAExpHelper)
           | _ => term_w_tlist
        )

      and nextLAExp (tlist : Token.token list) :
        AST.exp * toklist =
        nextLAExpHelper (nextBORExp (tlist))

      
      and nextBORExpHelper (term_w_tlist : AST.exp * toklist) :
        AST.exp * (toklist) =
        (case term_w_tlist
          of (term, T.BOr :: rest) =>
            nextGenExp (term, rest, nextBXORExp, AST.BOr, nextBORExpHelper)
           | _ => term_w_tlist
        )

      and nextBORExp (tlist : Token.token list) :
        AST.exp * toklist =
        nextBORExpHelper (nextBXORExp (tlist))

      
      and nextBXORExpHelper (term_w_tlist : AST.exp * toklist) :
        AST.exp * (toklist) =
        (case term_w_tlist
          of (term, T.BXor :: rest) =>
            nextGenExp (term, rest, nextBANDExp, AST.BXor, nextBXORExpHelper)
           | _ => term_w_tlist
        )

      and nextBXORExp (tlist : Token.token list) :
        AST.exp * toklist =
        nextBXORExpHelper (nextBANDExp (tlist))
      
      and nextBANDExpHelper (term_w_tlist : AST.exp * toklist) :
        AST.exp * (toklist) =
        (case term_w_tlist
          of (term, T.BAnd :: rest) =>
            nextGenExp (term, rest, nextEQExp, AST.BAnd, nextBANDExpHelper)
           | _ => term_w_tlist
        )

      and nextBANDExp (tlist : Token.token list) :
        AST.exp * toklist =
        nextBANDExpHelper (nextEQExp (tlist))
      
      and nextEQExpHelper (term_w_tlist : AST.exp * toklist) :
        AST.exp * (toklist) =
        (case term_w_tlist
          of (term, T.Eq :: rest) =>
            nextGenExp (term, rest, nextRELExp, AST.Eq, nextEQExpHelper)
           | (term, T.Neq :: rest) =>
            nextGenExp (term, rest, nextRELExp, AST.Neq, nextEQExpHelper)
           | _ => term_w_tlist
        )

      and nextEQExp (tlist : Token.token list) :
        AST.exp * toklist =
        nextEQExpHelper (nextRELExp (tlist))
      
      and nextRELExpHelper (term_w_tlist : AST.exp * toklist) :
        AST.exp * (toklist) =
        (case term_w_tlist
          of (term, T.Lt :: rest) =>
            nextGenExp (term, rest, nextBSExp, AST.Lt, nextRELExpHelper)
           | (term, T.Leq :: rest) =>
            nextGenExp (term, rest, nextBSExp, AST.Leq, nextRELExpHelper)
           | (term, T.Geq :: rest) =>
            nextGenExp (term, rest, nextBSExp, AST.Geq, nextRELExpHelper)
           | (term, T.Gt :: rest) =>
            nextGenExp (term, rest, nextBSExp, AST.Gt, nextRELExpHelper)
           | _ => term_w_tlist
        )

      and nextRELExp (tlist : Token.token list) :
        AST.exp * toklist =
        nextRELExpHelper (nextBSExp (tlist))
      
      and nextBSExpHelper (term_w_tlist : AST.exp * toklist) :
        AST.exp * (toklist) =
        (case term_w_tlist
          of (term, T.BLeft :: rest) =>
            nextGenExp (term, rest, nextAExp, AST.BLeft, nextBSExpHelper)
           | (term, T.BRight:: rest) =>
            nextGenExp (term, rest, nextAExp, AST.BRight, nextBSExpHelper)
           | _ => term_w_tlist
        )

      and nextBSExp (tlist : Token.token list) :
        AST.exp * toklist =
        nextBSExpHelper (nextAExp (tlist))

      and nextAExpHelper (term_w_tlist : AST.exp * toklist) :
        AST.exp * (toklist) =
        (case term_w_tlist
          of (term, T.Plus :: rest) =>
            nextGenExp (term, rest, nextTerm, AST.Plus, nextAExpHelper)
           | (term, T.Minus :: rest) =>
            nextGenExp (term, rest, nextTerm, AST.Minus, nextAExpHelper)
           | _ => term_w_tlist
        )

      and nextAExp (tlist : Token.token list) :
        AST.exp * toklist =
        nextAExpHelper (nextTerm (tlist))
          
      and nextTermHelper (factor_w_tlist :  AST.exp * toklist) :
           (AST.exp * toklist) =
        (case factor_w_tlist
          of (factor, T.Times :: rest) =>
            nextGenExp (factor, rest, nextFactor, AST.Times, nextTermHelper)
           | (factor, T.Div :: rest) =>
            nextGenExp (factor, rest, nextFactor, AST.Div, nextTermHelper)
           | (factor, T.Mod :: rest) =>
            nextGenExp (factor, rest, nextFactor, AST.Mod, nextTermHelper)
           | _ => factor_w_tlist
        )

      and nextTerm (tlist : Token.token list):
           (AST.exp * toklist) =
         nextTermHelper (nextFactor (tlist))
         
      and nextFactor(tlist : Token.token list) :
            (AST.exp * toklist) =
            (case tlist
              of T.OPar :: rest =>
                let
                  val exp_w_toks = nextExp rest
                in
                  (case exp_w_toks
                    of (exp, (T.CPar :: toks)) =>
                      (exp, toks)
                     | _ => raise Fail "Parse error, closing ) missing"
                  )
                end
                | T.Minus :: rest =>
                  let
                    val exp_w_toks = nextFactor rest
                  in
                    (case exp_w_toks
                      of (exp, toks) => (AST.UnOp (AST.Negation, exp), toks)
                    )
                  end
                | T.Complement :: rest =>
                  let
                    val exp_w_toks = nextFactor rest
                  in
                    (case exp_w_toks
                      of (exp, toks) => (AST.UnOp (AST.Complement, exp), toks)
                    )
                  end
                | T.Not :: rest =>
                  let
                    val exp_w_toks = nextFactor rest
                  in
                    (case exp_w_toks
                      of (exp, toks) => (AST.UnOp (AST.Not, exp), toks)
                    )
                  end
                | (T.IntLiteral num :: rest) => (AST.Const num, rest)
                | (T.Identifier s :: rest) => (AST.Var s, rest)
                | _ => raise Fail "Parse error, could not parse factor"
             )
      and nextStatement (tlist : Token.token list) :
        (AST.statement * toklist) =
        (case tlist
           of (T.KW k) :: rest =>
              (case k
                of T.Return =>
                  let
                    val exp_w_toks = nextExp rest
                  in
                    (case exp_w_toks
                       of (exp, (T.Semcol :: toks)) =>
                            (AST.Return (exp), toks)
                        | _ => raise Fail "Parse error on return, ending ; missing"
                    )
                 end
                  | T.Int => raise Fail "Not a statement"
              )
            | _ =>
              let
                val exp_w_toks = nextExp tlist
              in
                (case exp_w_toks
                  of (exp, (T.Semcol :: toks)) =>
                    (AST.Exp exp, toks)
                   | _ => raise Fail "Parse error, ending ; missing"
                )
              end
        )
      and nextDeclaration (tlist : toklist) : AST.declaration * toklist =
        (case tlist
          of (T.KW k) :: rest =>
            (case k
              of T.Int =>
                (case rest
                      of T.Identifier s :: rest' =>
                        (case rest'
                          of T.Semcol :: rest2 => 
                              (AST.Declare (AST.Int, s, NONE), rest2)
                           | T.Asgn :: rest2 => 
                              let
                                val exp_w_toks = nextExp rest2
                              in
                                (case exp_w_toks
                                  of (exp, (T.Semcol :: toks)) =>
                                    ((AST.Declare (AST.Int, s, SOME exp)), toks)
                                    | _ => raise Fail "Parse error, ending ; missing"
                                )
                              end
                            | _ => raise Fail 
                            "Parse error, variable declaration invalid"
                        )
                        | _ => raise Fail "Dangling int keyword"
                    )
                | _ => raise Fail "Not a declaration"
              )
        )
      and nextBlockItem (tlist : toklist) : (AST.block_item * toklist) =
        (case tlist
          of (T.KW k) :: rest =>
            (case k
              of T.Int =>
                let
                  val nextDecl_w_toks = nextDeclaration tlist
                in
                  (case nextDecl_w_toks
                    of (declaration, toks) =>
                      (AST.Declaration declaration, toks)
                  )
                end
              | _  =>
                let
                  val nextStatement_w_toks = nextStatement tlist
                in
                  (case nextStatement_w_toks
                    of (statement, toks) =>
                      (AST.Statement statement, toks)
                  )
                end
            )
        )
      and nextBlockItemListHelper (sts_w_tlist : AST.block_item list * toklist) :
        (AST.block_item list * toklist) =
        (case sts_w_tlist
          of (block_items, tlist) =>
            let
              val block_item_w_toks = nextBlockItem tlist
            in
              (case block_item_w_toks
                of (block_item, new_toks) =>
                  (case new_toks
                    of (T.CBrac :: rest) => ((block_items @ [block_item]), rest)
                    | _ => nextBlockItemListHelper (block_items @ [block_item],
                                                    new_toks)
                  )
              )
            end
        )
      and nextBlockItemList (tlist : Token.token list) :
        (AST.block_item list * toklist) =
        nextBlockItemListHelper ([], tlist)
    in
      (case tlist
        of [] => []
          | _ =>
              let
                val next_fun_w_rest = nextFun tlist
              in
                (case next_fun_w_rest
                   of (func, rest) => func :: (progAST rest)
               )
              end
      )
    end

end
