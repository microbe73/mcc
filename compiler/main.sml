structure Main : sig
  val fstring : TextIO.instream -> string
  val compile : string * string -> unit
  val main : string * string list -> OS.Process.status
end = struct

  fun fstring (strm : TextIO.instream) : string =
    let
      val newline = TextIO.inputLine strm
    in
      (case newline
        of NONE => ""
        | SOME st => st ^ fstring strm
      )
    end
  fun compile (files : string * string) : unit =
    let
      val infile = #1(files)
      val outfile = #2(files)
      val instream = TextIO.openIn infile
      val w = fstring instream
      val x = Scan.scan_toks w
      val y = Parse.progAST x
      val toplevel = TypeCheck.validate (AST.Prog y, TypeCheck.empty_store)
      val z = Generate.generate y
      val outstream = TextIO.openOut outfile
      val _ = TextIO.output (outstream, z)
      val _ = TextIO.closeOut outstream
    in
      ()
    end
    fun main (s : string * string list) : OS.Process.status =
      (case s
        of (nm, infile :: outfile :: rest) =>
          let
            val _ = compile (infile, outfile)
          in
            OS.Process.success
          end
         | _ => OS.Process.failure
      )

end
