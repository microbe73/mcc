structure Main = struct

  fun fstring (strm : TextIO.instream) : string =
    let
      val newline = TextIO.inputLine strm
    in
      (case newline
        of NONE => ""
        | SOME st => st ^ fstring strm
      )
    end
  fun main (files : string * string) : unit =
    let
      val infile = #1(files)
      val outfile = #2(files)
      val instream = TextIO.openIn infile
      val w = fstring instream
      val w2 = TextIO.print w
      val x = Scan.scan_toks w
      val y = Parse.progAST x
      val z = Generate.generate y
      val outstream = TextIO.openOut outfile
      val _ = TextIO.output (outstream, z)
      val _ = TextIO.closeOut outstream
    in
      ()
    end
end
