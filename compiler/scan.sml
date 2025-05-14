structure Scan = struct
    structure RE = RegExpFn(
      structure P = AwkSyntax
      structure E = ThompsonEngine
      (* structure E = BackTrackEngine *)
      (* structure E = DfaEngine *)
    )
  structure MT = MatchTree
  fun matched (plc : Token.token) matcher =
    (plc, matcher)
  fun getlen {len: int, pos:StringCvt.cs} : int =
    len
  fun scan (s : string) : Token.token * string =
        let
        
        val regexes = [
          ("{",                   fn match => matched Token.OBrac match),
          ("}",                   fn match => matched Token.CBrac match),
          (";",                   fn match => matched Token.Semcol match),
          ("\\(",                 fn match => matched Token.OPar match),
          ("\\)",                 fn match => matched Token.CPar match),
          ("int",                 fn match => matched (Token.KW Token.Int) match),
          ("return",              fn match => matched (Token.KW Token.Return) match),
          ("!",                   fn match => matched (Token.Not) match),
          ("~",                   fn match => matched (Token.Complement) match),
          ("-",                   fn match => matched (Token.Minus) match),
          ("\\+",                 fn match => matched (Token.Plus) match),
          ("\\*",                 fn match => matched (Token.Times) match),
          ("/",                   fn match => matched (Token.Div) match),

          ("&&",                  fn match => matched (Token.AND) match),
          ("\\|\\|",              fn match => matched (Token.OR) match),
          ("==",                  fn match => matched (Token.Eq) match),
          ("<=",                  fn match => matched (Token.Leq) match),
          ("<",                   fn match => matched (Token.Lt) match),
          (">=",                  fn match => matched (Token.Geq) match),
          (">",                   fn match => matched (Token.Gt) match),
          ("!=",                  fn match => matched (Token.Neq) match),
          ("[a-zA-Z][a-zA-Z0-9]*",
                                  fn match => matched (Token.Identifier "") match ),
          ("[0-9]+",              fn match => matched (Token.IntLiteral 0) match),
          ("[ \t\n]+",            fn match => matched Token.WS match)
        ]
        val match_result = StringCvt.scanString (RE.match regexes) s
        (* {len : int, pos : StringCvt.cs } MatchTree.match_tree *)
      in
        (**
         * StringCvt.scanString will traverse the `input` string and apply
         * the result of `RE.match regexes` to each character in the string.
         *
         * It's sort of a streaming matching process. The end result, however,
         * depends on your implementation above, in the match functions.
         *)
         (case match_result
            of NONE => raise Fail "Syntax error, unable to parse"
             | SOME (token, result_root) =>
                 let
                   val result = MatchTree.root (result_root)
                   val len = getlen result
                   val new_str = String.extract (s, len, NONE)
                 in
                   (case token
                      of Token.Identifier emp =>
                           (Token.Identifier (String.substring (s, 0, len)),
                            new_str)
                       | Token.IntLiteral emp =>
                           let
                             val substr = String.substring (s, 0, len)
                             val num = Int.fromString substr
                           in
                             (case num
                                of NONE => raise Fail "could not parse int"
                                 | SOME n => (Token.IntLiteral n, new_str)
                             )
                           end
                       | _ => (token, new_str)
                   )
                 end
         )
      end
  fun scan_toks (str : string) : Token.token list =
    (case str
       of "" => []
        | _ =>
            let
              val res = scan str
              val nexttok = #1(res)
              val newstr = #2(res)
            in
              (case nexttok
                 of Token.WS => scan_toks (newstr)
                  | _ => nexttok :: scan_toks (newstr)
              )
            end
    )
    
end
