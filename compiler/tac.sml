structure TAC : sig
  type quadruple
  type oper
  type arg
  val convert : AST.prog -> quadruple list
end = struct

  structure VM = VarMap
  datatype oper
  = UnOp of AST.un_operator
  | BinOp of AST.bin_operator
  | Call of string * int
  | Param of string
  | Copy
  | Goto

  type symbol = {name : string, vartype : AST.typ, rel_addr : int}
  (* rel_addr is the offset from %rsp basically *)
  datatype arg
  = Var of symbol
  | Const of int * AST.typ
  | Target of string
  | Tmp of string

  type quadruple = {label : string option, o : oper option, arg1 : arg option, arg2 : arg
  option, result : arg option }

  (*https://smlnj.org/doc/smlnj-lib/Util/str-HashTable.html
   * In particular, using the copy function each time a new scope is created can
   * allow for unique symbol tables for each scope [but i don't love that idea]
   * Could also try the one symbol table strategy using findAndRemove
   * In general hash maps in this library work slightly unusually, almost every
   * function just returns a unit (so the tables are basically just usual
   * imperative tables) *)
  val label_count = ref 0
  fun new_label (a : unit) : string =
  let
    val _ = label_count := !label_count + 1
  in
    "L" ^ Int.toString (!label_count)
  end
  val var_count = ref 0
  fun new_var (a : unit) : string =
  let
    val _ = var_count := !var_count + 1
  in
    "!t" ^ Int.toString (!var_count)
  end

  val exc = Fail "key error"
  fun convert (prog : AST.prog) : quadruple list =
  let
    val sym_tbl = HashTable.mkTable (HashString.hashString, op =) (100, exc)
    fun con_exp (exp : AST.exp) : quadruple list =
      (case exp
        of AST.Const n => [{label=NONE, o=NONE, arg1=NONE, arg2=NONE,result=
        SOME (Const (n, AST.Int))}]
          | AST.UnOp (oper, exp1) =>
            let
              val nv = new_var ()
              val tac1 = con_exp exp1
              val arg_addr = #result(List.last tac1)
            in
              tac1 @ [{label=NONE, o=SOME (UnOp oper), arg1=arg_addr,
              arg2=NONE,result=SOME (Tmp nv)}]
            end
          | AST.BinOp (oper, exp1, exp2) =>
            let
              val nv = new_var ()
              val tac1 = con_exp exp1
              val arg_addr1 = #result(List.last tac1)
              val tac2 = con_exp exp2
              val arg_addr2 = #result(List.last tac2)
            in
              tac1 @ tac2 @ [{label=NONE, o=SOME (BinOp oper), arg1=arg_addr1,
              arg2=arg_addr2,result=SOME (Tmp nv)}]
            end
          | AST.Assign (vname, exp1) =>
            let
              val vinfo = HashTable.lookup sym_tbl vname
              val tac1 = con_exp exp1
              val arg_addr1 = #result(List.last tac1)
            in
              tac1 @ [{label=NONE, o=SOME Copy, arg1=arg_addr1, arg2=NONE,
              result = SOME (Var vinfo)}]
            end
          | AST.Var (vname) =>
            let
              val vinfo = HashTable.lookup sym_tbl vname
            in
              [{label=NONE, o=NONE, arg1=NONE, arg2=NONE,result= SOME (Var
              vinfo)}]
            end
      )
    and con_stm (stm : AST.statement) : quadruple list =
      (case stm
        of AST.If (exp, true_stm, NONE) =>
          let
            val false_label = new_label()
            val bool_exp = con_exp exp
            val body = con_stm true_stm
            val final_val = #result(List.last bool_exp)
          in
            bool_exp @ [{label=NONE, o=SOME Goto, arg1=final_val,
            arg2=NONE, result = SOME (Target false_label)}] @ body
          end
      )
  in
    []
  end

end
