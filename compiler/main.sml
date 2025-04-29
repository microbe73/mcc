structure Main = struct

  fun main (a : string) =
    let
      val w = "int main(){ return ~~!2; }"
      val x = Scan.scan_toks w
      val y = Parse.progAST x
    in
      (case y
         of (ast, rest) => Generate.generate [ast]
      )
    end
end
