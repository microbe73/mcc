structure Parse = struct

  structure T = Token
  type toklist = Token.token list
  fun progAST (tlist : Token.token list) : AST.func list =
    let
      fun nextFun (tlist : Token.token list) : (AST.func * (Token.token list))
        =
        (case tlist
           of T.KW (T.Int) :: T.Identifier fname :: T.OPar :: T.CPar :: T.OBrac ::
           rest =>
                   let
                     val next = nextStatement rest
                   in
                     (case next
                        of (statement, (T.CBrac :: toks)) =>
                             (AST.Fun (fname, statement), toks)
                               | _ => raise Fail "Parse error, closing } missing"
                     )
                   end
            | _ => raise Fail "Parse error, unable to find function declaration"

        )
      and nextExpHelper (term_w_tlist : AST.exp * toklist) :
        AST.exp * (toklist) =
        (case term_w_tlist
          of (term, T.Plus :: rest) =>
            let
              val next_term = nextTerm rest
            in
              (case next_term
                of (nterm, ntoks) =>
                  nextExpHelper ((AST.BinOp (AST.Plus, term, nterm)), ntoks)
              )
            end
           | (term, T.Minus :: rest) =>
            let
              val next_term = nextTerm rest
            in
            (case next_term
                of (nterm, ntoks) =>
                  nextExpHelper ((AST.BinOp (AST.Minus, term, nterm)), ntoks)
            )
            end
           | _ => term_w_tlist
        )
      and nextExp (tlist : Token.token list) :
        AST.exp * toklist =
        nextExpHelper (nextTerm (tlist))

          
      and nextTermHelper (factor_w_tlist :  AST.exp * toklist) :
           (AST.exp * toklist) =
        (case factor_w_tlist
          of (factor, T.Times :: rest) =>
            let
              val next_factor = nextFactor rest
            in
              (case next_factor
                of (nfactor, ntoks) =>
                  nextTermHelper ((AST.BinOp (AST.Times, factor, nfactor)), ntoks)
              )
            end
           | (factor, T.Div :: rest) =>
            let
              val next_factor = nextFactor rest
            in
            (case next_factor
                of (nfactor, ntoks) =>
                  nextTermHelper ((AST.BinOp (AST.Div, factor, nfactor)), ntoks)
            )
            end
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
                | _ => raise Fail "Parse error, could not parse factor"
             )
      and nextStatement (tlist : Token.token list) :
        (AST.statement * toklist) =
        (case tlist
           of T.KW Return :: rest =>
                let
                  val exp_w_toks = nextExp rest
                in
                  (case exp_w_toks
                     of (exp, (T.Semcol :: toks)) =>
                          (AST.Return (exp), toks)
                      | _ => raise Fail "Parse error, closing ; missing"
                  )
                end
            | _ => raise Fail "todo"
        )
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
