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
  fun matched2 plc matcher = matcher
  fun scan s =
          let
        (**
         * A list of (regexp, match function) pairs. The function called by
         * RE.match is the one associated with the regexp that matched.
         *
         * The match parameter is described here:
         * 
         *)
        fun full_name{first:string,last:string,age:int,balance:real}:string =
          raise Fail "todo"
        fun handler{len : int, pos : StringCvt.cs } :
          string =
          raise Fail "TODO"
        val regexes = [
          ("{",             fn match => matched2 Token.OBrac match),
          ("}",             fn match => matched2 Token.CBrac match),
          (";",             fn match => matched2 Token.Semcol match),
          ("\\(",           fn match => matched2 Token.OPar match),
          ("\\)",           fn match => matched2 Token.CPar match),
          ("int",           fn match => matched2 (Token.KW Token.Int) match),
          ("return",        fn match => matched2 (Token.KW Token.Return) match),
          ("[a-zA-Z][a-zA-Z0-9]*",
                            fn match => matched2 (Token.Identifier "") match ),
          ("[0-9]+",        fn match => matched2 (Token.IntLiteral 0) match),
          ("[ \t\n]+",      fn match => matched2 Token.WS match)
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
            of NONE => raise Fail "todo"
             | SOME result => MatchTree.root (result)
         )
      end
end
