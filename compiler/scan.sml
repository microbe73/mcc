structure Scan = struct
    structure RE = RegExpFn(
      structure P = AwkSyntax
      structure E = ThompsonEngine
      (* structure E = BackTrackEngine *)
      (* structure E = DfaEngine *)
    )
  fun matched (plc : string) matcher =
    (plc, matcher)
  fun scan s =
          let
        (**
         * A list of (regexp, match function) pairs. The function called by
         * RE.match is the one associated with the regexp that matched.
         *
         * The match parameter is described here:
         * 
         *)
        val regexes = [
          ("[a-zA-Z]*",   fn match => matched "1st" match),
          ("[0-9]*",      fn match => matched "2nd" match),
          ("1tom|2jerry", fn match => matched "3rd" match)
        ]
      in
        (**
         * StringCvt.scanString will traverse the `input` string and apply
         * the result of `RE.match regexes` to each character in the string.
         *
         * It's sort of a streaming matching process. The end result, however,
         * depends on your implementation above, in the match functions.
         *)
        StringCvt.scanString (RE.match regexes) s
      end
end
