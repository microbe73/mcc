structure VarMap : sig

  type pmap
  type var_info
  val empty_map : pmap

  val find   : string * pmap -> var_info (* find value in map *)
  val contains : string * pmap -> bool (* check if value is in map *)
  val ins   : (string * var_info) * pmap -> pmap  (* add value to map *)                          
  val rem   : string * pmap -> pmap  (* remove value from map *)
end = struct

  type var_info = int

  type pmap = (string * var_info) list

  val empty_map = []

  fun find (inp : string * pmap) : var_info =
    let
      val name = #1(inp)
      val vpmap = #2(inp)
    in
      (case vpmap
         of [] => raise Fail "variable used but not in map"
          | (s :: rest) =>
          (case s
            of (var, info) => if var = name then info else find (name, rest)
          )
      )
    end
  fun contains (inp : string * pmap) : bool =
    let
      val name = #1(inp)
      val pmap = #2(inp)
    in
      (case pmap
        of [] => false
        | (s :: rest) =>
          (case s
            of (var, info) => if var = name then true else contains (name, rest)
          )
      )
    end
  fun ins (inp : (string * var_info) * pmap) : pmap =
    let
      val name_w_info = #1(inp)
      val vpmap = #2(inp)
    in
      name_w_info :: vpmap
    end

  fun rem (inp : string * pmap) : pmap =
    let
      val pmap = #2(inp)
      val name = (#1(inp), find (#1(inp), pmap))
      fun isname n = n <> name
    in
      List.filter isname pmap
    end

  fun union (s: pmap * pmap) : pmap = #1(s) @ #2(s)
end


