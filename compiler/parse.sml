structure Parse = struct

  structure T = Token
  type toklist = Token.token list
  type binlist = AST.bin_operator list
  fun type_token (t : Token.type_id) : AST.typ =
    (case t
      of T.Int => AST.Int
    )

  fun progAST (all_toks : Token.token list) : AST.func list =
    let
      fun nextFun (tlist : Token.token list) : (AST.func * (Token.token list))
        =
        (case tlist
           of T.TyID t :: T.Identifier fname :: T.OPar :: rest =>
                   let
                     val (args, new_toks) = nextArgs ([], rest)
                   in
                     (case new_toks
                      of T.Semcol :: rest2 =>
                           (AST.Fun (fname, args, NONE, (type_token t)) , rest2)
                        | T.OBrac :: rest2 =>
                            let
                              val (body, new_toks2) = nextBlockItemList rest2
                            in
                              (AST.Fun (fname, args, SOME body, (type_token t)),
                              new_toks2)
                            end
                        | _ => raise Fail ("Invalid function declaration `" ^
                        fname ^ "` (does not end with either ; or {)")
                      )
                   end
            | _ => raise Fail "Parse error, unable to find function declaration"

        )
      and nextArgs (args_w_tlist : ((string * AST.typ) list) * toklist)
        : ((string * AST.typ) list) * toklist =
        let
          val (args, tlist) = args_w_tlist
        in
          (case tlist
            of T.TyID t :: T.Identifier argname :: rest =>
                 let
                   val new_args = args @ [(argname, (type_token t))]
                 in
                   (case rest
                      of T.Comma :: rest' =>
                          nextArgs (new_args, rest')
                       | T.CPar :: rest' =>
                           (new_args, rest')
                       | _ => raise Fail
                       ("error while parsing function declaration argument`" ^
                       argname ^ "` (missing ')' or ',')")
                   )
                 end
             | T.CPar :: rest => ([], rest)
             | _ => 
                 raise Fail "error while attempting to parse function arguments"
          )
        end
      and nextGenExp (ops_w_lvl : AST.exp * toklist * (toklist -> AST.exp * toklist) * AST.bin_operator *
      (AST.exp * toklist -> AST.exp * toklist)) : AST.exp * toklist =
        (case ops_w_lvl
          of (term, toks, nextPrecFn, binop, caller) =>
            let
              val (nterm, ntoks) = nextPrecFn toks
            in
              caller ((AST.BinOp (binop, term, nterm)), ntoks)
            end
        )
      and nextExp (tlist : toklist) : AST.exp * toklist =
        (case tlist
          of T.Identifier s :: T.Asgn :: rest =>
            let
              val (next, toks) = nextExp rest
            in
               (AST.Assign (s, next), toks)
            end
           | _ => nextCondExp tlist
        )
(*Assignment/Assignment operators like +=*)
      and nextCondExp (tlist : toklist) : AST.exp * toklist =
        let
          val (cond, toks) = nextLOExp tlist
        in
          (case toks
            of (T.Question :: rest) =>
              let
                val exp1_w_toks = nextExp rest
              in
                (case exp1_w_toks
                  of (exp1, toks2) =>
                    (case toks2
                      of (T.Colon :: rest2) =>
                        let
                          val exp2_w_toks = nextCondExp rest2
                        in
                          (case exp2_w_toks
                            of (exp2, toks3) =>
                              (AST.Conditional (cond, exp1, exp2), toks3)
                          )
                        end
                      | _ => raise Fail "? conditional without :"
                    )
                )
              end
             | _ => (cond, toks)
          )
        end
      and nextLOExpHelper (term_w_tlist : AST.exp * toklist) :
        AST.exp * toklist =
        (case term_w_tlist
          of (term, T.OR :: rest) =>
            nextGenExp (term, rest, nextLAExp, AST.OR, nextLOExpHelper)
           | _ => term_w_tlist
        )

      and nextLOExp (tlist : toklist) :
        AST.exp * toklist =
        nextLOExpHelper (nextLAExp tlist)

      and nextLAExpHelper (term_w_tlist : AST.exp * toklist) :
        AST.exp * toklist =
        (case term_w_tlist
          of (term, T.AND :: rest) =>
            nextGenExp (term, rest, nextBORExp, AST.AND, nextLAExpHelper)
           | _ => term_w_tlist
        )

      and nextLAExp (tlist : Token.token list) :
        AST.exp * toklist =
        nextLAExpHelper (nextBORExp tlist)

      and nextBORExpHelper (term_w_tlist : AST.exp * toklist) :
        AST.exp * toklist =
        (case term_w_tlist
          of (term, T.BOr :: rest) =>
            nextGenExp (term, rest, nextBXORExp, AST.BOr, nextBORExpHelper)
           | _ => term_w_tlist
        )

      and nextBORExp (tlist : Token.token list) :
        AST.exp * toklist =
        nextBORExpHelper (nextBXORExp tlist)

      and nextBXORExpHelper (term_w_tlist : AST.exp * toklist) :
        AST.exp * toklist =
        (case term_w_tlist
          of (term, T.BXor :: rest) =>
            nextGenExp (term, rest, nextBANDExp, AST.BXor, nextBXORExpHelper)
           | _ => term_w_tlist
        )

      and nextBXORExp (tlist : Token.token list) :
        AST.exp * toklist =
        nextBXORExpHelper (nextBANDExp tlist)

      and nextBANDExpHelper (term_w_tlist : AST.exp * toklist) :
        AST.exp * (toklist) =
        (case term_w_tlist
          of (term, T.BAnd :: rest) =>
            nextGenExp (term, rest, nextEQExp, AST.BAnd, nextBANDExpHelper)
           | _ => term_w_tlist
        )

      and nextBANDExp (tlist : Token.token list) :
        AST.exp * toklist =
        nextBANDExpHelper (nextEQExp tlist)

      and nextEQExpHelper (term_w_tlist : AST.exp * toklist) :
        AST.exp * toklist =
        (case term_w_tlist
          of (term, T.Eq :: rest) =>
            nextGenExp (term, rest, nextRELExp, AST.Eq, nextEQExpHelper)
           | (term, T.Neq :: rest) =>
            nextGenExp (term, rest, nextRELExp, AST.Neq, nextEQExpHelper)
           | _ => term_w_tlist
        )

      and nextEQExp (tlist : Token.token list) :
        AST.exp * toklist =
        nextEQExpHelper (nextRELExp tlist)

      and nextRELExpHelper (term_w_tlist : AST.exp * toklist) :
        AST.exp * toklist =
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
        nextRELExpHelper (nextBSExp tlist)

      and nextBSExpHelper (term_w_tlist : AST.exp * toklist) :
        AST.exp * toklist =
        (case term_w_tlist
          of (term, T.BLeft :: rest) =>
            nextGenExp (term, rest, nextAExp, AST.BLeft, nextBSExpHelper)
           | (term, T.BRight:: rest) =>
            nextGenExp (term, rest, nextAExp, AST.BRight, nextBSExpHelper)
           | _ => term_w_tlist
        )

      and nextBSExp (tlist : Token.token list) :
        AST.exp * toklist =
        nextBSExpHelper (nextAExp tlist)

      and nextAExpHelper (term_w_tlist : AST.exp * toklist) :
        AST.exp * toklist =
        (case term_w_tlist
          of (term, T.Plus :: rest) =>
            nextGenExp (term, rest, nextTerm, AST.Plus, nextAExpHelper)
           | (term, T.Minus :: rest) =>
            nextGenExp (term, rest, nextTerm, AST.Minus, nextAExpHelper)
           | _ => term_w_tlist
        )

      and nextAExp (tlist : Token.token list) :
        AST.exp * toklist =
        nextAExpHelper (nextTerm tlist)

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
         nextTermHelper (nextFactor tlist)

      and nextFactor (tlist : Token.token list) :
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
                | (T.Identifier fname :: T.OPar :: rest) =>
                    let
                      val (call_args, new_toks) = nextCallArgs ([], rest)
                    in
                      (AST.FunCall (fname, call_args), new_toks)
                    end
                | (T.Identifier s :: rest) => (AST.Var s, rest)
                | (T.Semcol :: _) => raise Fail "Parsing ;"
                | _ => raise Fail
                ("Parse error, could not parse factor " ^
                T.toString (List.hd tlist))
             )
      and nextCallArgs (args_w_tlist : AST.exp list * toklist) : (AST.exp list *
        toklist) =
        let
          val (args, tlist) = args_w_tlist
        in
          (case tlist
            of T.CPar :: rest => (args, rest)
              | _ =>
                  let
                    val (exp, new_toks) = nextExp tlist
                  in
                    (case new_toks
                       of T.Comma :: rest =>
                        nextCallArgs (args @ [exp], rest)
                        | T.CPar :: rest =>
                            (args @ [exp], rest)
                        | tok :: _ => raise Fail ("error parsing function call args. " ^
                        "Expected ',' or ')', found " ^ T.toString tok)
                        | [] => raise Fail ("error parsing function call args" ^
                        "(found EOF instead)")
                    )
                  end
          )
        end
      and nextStatement (tlist : Token.token list) :
        (AST.statement * toklist) =
        (case tlist
           of T.Return :: rest =>
              let
                val (exp, toks) = nextExp rest
              in
                (case toks
                   of (T.Semcol :: toks) =>
                        (AST.Return (exp), toks)
                    | _ => raise Fail "Parse error on return, ending ; missing"
                )
             end
            | T.If :: T.OPar :: rest =>
              let
                val (exp, toks) = nextExp rest
              in
                (case toks
                  of T.CPar :: toks =>
                    let
                      val (stm, toks') = nextStatement toks
                    in
                      (case toks'
                        of T.Else :: new_toks =>
                          let
                            val elseCond_w_toks = nextStatement new_toks
                          in
                            (case elseCond_w_toks
                              of (elseStm, new_toks2) =>
                                (AST.If (exp, stm, SOME (elseStm)), new_toks2)
                            )
                          end
                         | _ =>
                          (AST.If (exp, stm, NONE), toks')
                      )
                    end
                    | _ => raise Fail "If conditional ) missing"
                )
              end
            | T.OBrac :: rest =>
              let
                val (blockItems, toks) = nextBlockItemList rest
              in
                (AST.Compound blockItems, toks)
              end
            | T.For :: T.OPar :: rest =>
                nextFor rest (*TODO: Make nextFor not look horrifying*)
            | T.While :: T.OPar :: rest =>
                let
                  val (exp, toks) = nextExp rest
                in
                  (case toks
                    of T.CPar :: rest' =>
                      let
                        val (statement, toks') = nextStatement rest'
                      in
                        (AST.While (exp, statement), toks')
                      end
                     | _ => raise Fail "While condition missing )"
                  )
                end
            | T.Do :: rest =>
                let
                  val (statement, toks) = nextStatement rest
                in
                  (case toks
                     of T.While :: T.OPar :: rest' =>
                      let
                        val (exp, toks') = nextExp rest'
                      in
                        (case toks'
                          of T.CPar :: toks =>
                            (AST.Do (statement, exp), toks)
                           | _ => raise Fail "Do cond missing )"
                        )
                      end
                      | _ => raise Fail "Do statement unterminated"
                  )
                end
            | T.Break :: rest =>
                (case rest
                  of T.Semcol :: rest' => (AST.Break, rest')
                    | _ => raise Fail "Break statement missing ;"
                )
            | T.Continue :: rest =>
                (case rest
                  of T.Semcol :: rest' => (AST.Continue, rest')
                    | _ => raise Fail "Continue statement missing ;"
                )
            | _ =>
              let
                val exp_w_toks = nextExp tlist
              in
                (case exp_w_toks
                  of (exp, (T.Semcol :: toks)) =>
                    (AST.Exp (SOME exp), toks)
                   | _ => raise Fail "Parse error, ending ; missing"
                )
              end
        )
      and nextDeclaration (tlist : toklist) : AST.declaration * toklist =
        (case tlist
          of (T.TyID t) :: rest =>
            (case t
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
                            ("Parse error, variable`" ^ s ^
                            "` declaration invalid")
                        )
                        | _ => raise Fail "Dangling int keyword"
                    )
              )
            | _ => raise Fail "not a declaration"
        )
      and nextBlockItem (tlist : toklist) : (AST.block_item * toklist) =
        (case tlist
          of (T.TyID k) :: rest =>
            (case k
              of T.Int =>
                let
                  val (declaration, toks) = nextDeclaration tlist
                in
                  (AST.Declaration declaration, toks)
                end
            )
          | _ =>
              let
                val (statement, toks) = nextStatement tlist
              in
                (AST.Statement statement, toks)
              end
        )
      and nextBlockItemListHelper (sts_w_tlist : AST.block_item list * toklist) :
        (AST.block_item list * toklist) =
        (case sts_w_tlist
          of (block_items, tlist) =>
            let
              val (block_item, new_toks) = nextBlockItem tlist
            in
              (case new_toks
                of (T.CBrac :: rest) => ((block_items @ [block_item]), rest)
                | _ => nextBlockItemListHelper (block_items @ [block_item],
                                                new_toks)
              )
            end
        )
      and nextBlockItemList (tlist : Token.token list) :
        (AST.block_item list * toklist) =
        nextBlockItemListHelper ([], tlist)
      and nextFor (rest : toklist) : AST.statement * toklist =
        (case rest
          of (T.Semcol :: rest') =>
               (case rest'
                  of (T.Semcol :: rest'') =>
                    (case rest''
                      of (T.CPar :: rest''') =>
                           let
                             val (body, toks) = nextStatement rest'''
                           in
                             (AST.For (NONE, AST.Const 1, NONE,
                              body), rest''')
                           end
                        | _ =>
                            let
                              val (exp2, rest) = nextExp rest''
                            in
                              (case rest
                                of T.CPar :: rest''' =>
                                  let
                                    val (body, toks) = nextStatement rest'''
                                  in
                                    (AST.For (NONE, AST.Const 1, SOME exp2,
                                     body), toks)
                                  end
                                | _ =>
                                raise Fail "For loop unclosed paren"
                              )
                            end
                    )
                   | _ =>
                    let
                      val (exp1, rest) = nextExp rest'
                    in
                      (case rest
                        of T.Semcol :: rest'' =>
                             (case rest''
                              of T.CPar :: rest''' =>
                                 let
                                   val (body, toks) = nextStatement rest'''
                                 in
                                   (AST.For (NONE, exp1, NONE, body), rest''')
                                 end
                              | _ =>
                                  let
                                    val (exp2, rest) = nextExp rest''
                                  in
                                    (case rest
                                      of T.CPar :: rest''' =>
                                        let
                                          val (body, toks) = nextStatement
                                          rest'''
                                        in
                                          (AST.For (NONE, exp1, SOME exp2,
                                           body), toks)
                                        end
                                      | _ =>
                                      raise Fail "For loop unclosed paren"
                                    )
                                  end
                                )
                          | _ =>
                            raise Fail "For loop unterminated expression"
                      )
                    end
               )
           | _ =>
            let
              val blockItem_w_toks = nextBlockItem rest
            in
              (case blockItem_w_toks
                of (AST.Declaration decl, rest') =>
                     (case rest'
                      of T.Semcol :: rest'' =>
                           (case rest''
                            of T.CPar :: rest''' =>
                                let
                                  val (body, toks) = nextStatement rest'''
                                in
                                  (AST.ForDecl (decl, AST.Const 1, NONE, body),
                                   toks)
                                end
                              | _ =>
                                  let
                                    val exp2_w_r = nextExp rest''
                                  in
                                    (case exp2_w_r
                                       of (exp2, T.CPar :: rest''') =>
                                            let
                                              val (body, toks) =
                                                nextStatement rest'''
                                            in
                                              (AST.ForDecl (decl, AST.Const 1,
                                               SOME exp2, body), toks)
                                            end
                                         | _ => raise Fail ("for loop" ^
                                         "missing )")
                                    )
                                  end
                           )
                       | _ =>
                           let
                             val exp1_w_r = nextExp rest'
                            in
                              (case exp1_w_r
                                of (exp1, T.Semcol :: rest'') =>
                                     (case rest''
                                        of (T.CPar :: rest''') =>
                                          let
                                            val (body, toks) =
                                              nextStatement rest'''
                                          in
                                            (AST.ForDecl (decl, exp1, NONE,
                                             body), toks)
                                          end
                                         | _ =>
                                            let
                                              val exp2_w_r = nextExp
                                              rest''
                                            in
                                              (case exp2_w_r
                                                 of (exp2, T.CPar ::
                                                 rest''') =>
                                                 let
                                                   val (body, toks) =
                                                     nextStatement
                                                     rest'''
                                                 in
                                                  (AST.ForDecl (decl, exp1, SOME
                                                  exp2, body), toks)
                                                 end
                                                 | _ => raise Fail
                                                 "for loop missing )"
                                              )
                                            end
                                     )
                                 | _ => raise Fail "missing ; in for loop"
                              )
                            end
                     )
                 | (AST.Statement stm, rest') =>
                     (case stm
                      of AST.Exp exp_option =>
                      (case rest'
                      of T.Semcol :: rest'' =>
                           (case rest''
                            of T.CPar :: rest''' =>
                                let
                                  val (body, toks) = nextStatement rest'''
                                in
                                  (AST.For (exp_option, AST.Const 1, NONE,
                                   body), toks)
                                end
                              | _ =>
                                  let
                                    val exp2_w_r = nextExp rest''
                                  in
                                    (case exp2_w_r
                                       of (exp2, T.CPar :: rest''') =>
                                            let
                                              val (body, toks) =
                                                nextStatement rest'''
                                            in
                                              (AST.For (exp_option, AST.Const 1,
                                               SOME exp2, body), toks)
                                            end
                                         | _ => raise Fail ("for loop" ^
                                         "missing )")
                                    )
                                  end
                           )
                       | _ =>
                           let
                             val exp1_w_r = nextExp rest'
                            in
                              (case exp1_w_r
                                of (exp1, T.Semcol :: rest'') =>
                                     (case rest''
                                        of (T.CPar :: rest''') =>
                                          let
                                            val (body, toks) =
                                              nextStatement rest'''
                                          in
                                            (AST.For (exp_option, exp1, NONE,
                                             body), toks)
                                          end
                                         | _ =>
                                            let
                                              val exp2_w_r = nextExp
                                              rest''
                                            in
                                              (case exp2_w_r
                                                 of (exp2, T.CPar ::
                                                 rest''') =>
                                                 let
                                                   val (body, toks) =
                                                     nextStatement
                                                     rest'''
                                                 in
                                                  (AST.For (exp_option, exp1,
                                                   SOME exp2, body), toks)
                                                 end
                                                 | _ => raise Fail
                                                 "for loop missing )"
                                              )
                                            end
                                     )
                                 | _ => raise Fail "missing ; in for loop"
                              )
                            end
                     )
                    | _ => raise Fail "Cannot put statement in for loop"
                  )
              )
            end
          )
    in
      (case all_toks
        of [] => []
          | _ =>
              let
                val next_fun_w_rest = nextFun all_toks
              in
                (case next_fun_w_rest
                   of (func, rest) => func :: (progAST rest)
               )
              end
      )
    end

end
