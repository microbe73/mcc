structure Parse = struct

  structure T = Token
  type toklist = Token.token list
  fun progAST (tlist : Token.token list) : AST.func * Token.token list =
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
            | _ => raise Fail "todo"

        )
      and nextExp (tlist : Token.token list) :
        AST.exp * (Token.token list) =
        (case tlist
           of (T.IntLiteral num :: rest) => (AST.Const num, rest)
            | _ => raise Fail "Parse error: could not parse expression"
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
      nextFun tlist
    end

end
