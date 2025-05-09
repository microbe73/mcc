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
      and nextExpHelper (tlist : T.token list) :
        AST.exp * (toklist) =
        let
          val term_w_toks = nextTerm(tlist)
        in
          (case term_w_toks
             of (term, T.Plus :: rest) =>
                let
                  val next_term_w_toks = nextExpHelper rest
                in
                  (case next_term_w_toks
                    of (next_term, new_toks) =>
                      (AST.BinOp (AST.Plus, term, next_term), new_toks)
                  )
                end
              | (term, T.Minus :: rest) =>
                let
                  val next_term_w_toks = nextExpHelper rest
                in
                  (case next_term_w_toks
                    of (next_term, new_toks) =>
                      (AST.BinOp (AST.Minus, term, next_term), new_toks)
                  )
                end
              | (term, rest) => (term, rest)
           )
         end
      and nextExp (tlist : Token.token list) :
        AST.exp * toklist =
        let
          val term_w_toks = nextTerm(tlist)
        in
          (case term_w_toks
            of (term, T.Plus :: rest) =>
              let
                val next_term_w_toks = nextExpHelper rest
              in
                (case next_term_w_toks
                   of (next_term, new_toks) =>
                    (AST.BinOp (AST.Plus, term, next_term), new_toks)
                )
              end
              | (term, T.Minus :: rest) =>
              let
                val next_term_w_toks = nextExpHelper rest
              in
                (case next_term_w_toks
                   of (next_term, new_toks) =>
                    (AST.BinOp (AST.Minus, term, next_term), new_toks)
                )
              end
              | (term, rest) => raise Fail "Unable to parse expression"
            )
        end
      and nextTermHelper (tlist : Token.token list) :
           (AST.exp * toklist) =
        let
          val factor_w_toks = nextFactor(tlist)
        in
          (case factor_w_toks
             of (factor, T.Times :: rest) =>
                let
                  val next_factor_w_toks = nextTermHelper rest
                in
                  (case next_factor_w_toks
                    of (next_factor, new_toks) =>
                      (AST.BinOp (AST.Times, factor, next_factor), new_toks)
                  )
                end
              | (factor, T.Div :: rest) =>
                let
                  val next_factor_w_toks = nextTermHelper rest
                in
                  (case next_factor_w_toks
                    of (next_factor, new_toks) =>
                      (AST.BinOp (AST.Div, factor, next_factor), new_toks)
                  )
                end
              | (factor, rest) => (factor, rest)
           )
         end
      and nextTerm (tlist : Token.token list):
           (AST.exp * toklist) =
        let
          val factor_w_toks = nextTerm(tlist)
        in
          (case factor_w_toks
            of (factor, T.Times :: rest) =>
              let
                val next_factor_w_toks = nextTermHelper rest
              in
                (case next_factor_w_toks
                   of (next_factor, new_toks) =>
                    (AST.BinOp (AST.Times, factor, next_factor), new_toks)
                )
              end
              | (factor, T.Div :: rest) =>
              let
                val next_factor_w_toks = nextTermHelper rest
              in
                (case next_factor_w_toks
                   of (next_factor, new_toks) =>
                    (AST.BinOp (AST.Div, factor, next_factor), new_toks)
                )
              end
              | (term, rest) =>
                  raise Fail "Unable to parse expression (look at a division)"
            )
        end
      and nextFactor(tlist : Token.token list) :
            (AST.exp * toklist) =
            raise Fail "todo"
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
(* case tlist
             of (T.IntLiteral num :: rest) => (AST.Const num, rest)
              | (T.Minus :: rest) =>
                  let
                    val rest_parsed = nextExp rest
                  in
                    (case rest_parsed
                       of (inner_exp, rest1) =>
                            (AST.UnOp (AST.Negation , inner_exp), rest1)
                    )
                  end
              | (T.Not :: rest) =>
                  let
                    val rest_parsed = nextExp rest
                  in
                    (case rest_parsed
                       of (inner_exp, rest1) =>
                            (AST.UnOp (AST.Not, inner_exp), rest1)
                    )
                  end
              | (T.Complement :: rest) =>
                  let
                    val rest_parsed = nextExp rest
                  in
                    (case rest_parsed
                       of (inner_exp, rest1) =>
                            (AST.UnOp (AST.Complement, inner_exp), rest1)
                    )
                  end
              | _ => raise Fail "Parse error: could not parse expression"
          *)
