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
  (* | CondGoto of rel_op *)

  type symbol = {name : string, vartype : AST.typ, rel_addr : int}
  (* rel_addr is the offset from %rsp basically *)
  datatype arg
  = Var of symbol
  | Const of int * AST.typ
  | Target of string
  | Tmp of string

  type quadruple = {label : string option, o : oper option, arg1 : arg option, arg2 : arg
  option, result : arg option }

  type stmInfo = {continue_label : string option, break_label : string option}
  (*https://smlnj.org/doc/smlnj-lib/Util/str-HashTable.html
   * In particular, using the copy function each time a new scope is created can
   * allow for unique symbol tables for each scope [but i don't love that idea]
   * Could also try the one symbol table strategy using findAndRemove
   * In general hash maps in this library work slightly unusually, almost every
   * function just returns a unit (so the tables are basically just usual
   * imperative tables) *)
  val label_count = ref 0
  fun new_label (_ : unit) : string =
  let
    val _ = label_count := !label_count + 1
  in
    "L" ^ Int.toString (!label_count)
  end
  val var_count = ref 0
  fun new_var (_ : unit) : string =
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
          | AST.Var vname =>
            let
              val vinfo = HashTable.lookup sym_tbl vname
            in
              [{label=NONE, o=NONE, arg1=NONE, arg2=NONE,result= SOME (Var
              vinfo)}]
            end
          | AST.Conditional (cond_exp, true_exp, false_exp) =>
            let
             val false_label = new_label ()
             val bool_exp = con_exp cond_exp
             val exp_val = #result (List.last bool_exp)
             val true_body = con_exp true_exp
             val false_body = con_exp false_exp
             val end_label = new_label ()
           in
             bool_exp @
             ({label=NONE, o=SOME Goto, arg1=exp_val, arg2=NONE, result = SOME
             (Target false_label)} ::
             true_body) @
             {label=NONE, o=SOME Goto, arg1=NONE, arg2=NONE, result=SOME
             (Target end_label)} ::
             {label=SOME false_label, o=NONE, arg1=NONE, arg2=NONE,
              result=NONE} ::
             false_body @
             [{label=SOME end_label, o=NONE, arg1=NONE, arg2=NONE, result=NONE}]
            end
          | AST.FunCall (_, _) => raise Fail "Implement function calls" (*TODO*)

      )
    and con_stm (stm_w_info : AST.statement * stmInfo) : quadruple list =

      case stm_w_info
        of (AST.If (exp, true_stm, NONE), info) =>
             let
               val false_label = new_label()
               val bool_exp = con_exp exp
               val body = con_stm (true_stm, info)
               val exp_val = #result(List.last bool_exp)
            in
              bool_exp @ {label=NONE, o=SOME Goto, arg1=exp_val,
              arg2=NONE, result = SOME (Target false_label)} :: body @
              [{label=SOME false_label, o=NONE, arg1=NONE, arg2=NONE, result=NONE}]
            end
         | (AST.If (exp, true_stm, SOME false_stm), info) =>
             let
               val false_label = new_label ()
               val bool_exp = con_exp exp
               val exp_val = #result (List.last bool_exp)
               val true_body = con_stm (true_stm, info)
               val false_body = con_stm (false_stm, info)
               val end_label = new_label ()
             in
               bool_exp @
               ({label=NONE, o=SOME Goto, arg1=exp_val, arg2=NONE, result = SOME
               (Target false_label)} ::
               true_body) @
               {label=NONE, o=SOME Goto, arg1=NONE, arg2=NONE, result=SOME
               (Target end_label)} ::
               {label=SOME false_label, o=NONE, arg1=NONE, arg2=NONE,
                result=NONE} ::
                false_body @
                [{label=SOME end_label, o=NONE, arg1=NONE, arg2=NONE, result=NONE}]
             end
         | (AST.While (exp, body), _) =>
             let
               val loop_label = new_label ()
               val end_label = new_label ()
               val bool_exp = con_exp exp
               val exp_val = #result (List.last bool_exp)
               val loop_body = con_stm (body, {continue_label=SOME loop_label,
               break_label=SOME end_label})
             in
               ({label=SOME loop_label, o=NONE, arg1=NONE, arg2=NONE, result=NONE} ::
               bool_exp) @
               ({label=NONE, o=SOME Goto, arg1=exp_val, arg2=NONE, result = SOME
               (Target end_label)} :: loop_body) @
               [{label=SOME end_label, o=NONE, arg1=NONE, arg2=NONE, result=NONE}]
             end
         | (AST.Do (body, exp), _) =>
             let
               val loop_label = new_label ()
               val end_label = new_label ()
               val bool_exp = con_exp exp
               val exp_val = #result (List.last bool_exp)
               val loop_body = con_stm (body, {continue_label=SOME loop_label,
               break_label=SOME end_label})
             in
               loop_body @
               [{label=SOME end_label, o=NONE, arg1=NONE, arg2=NONE, result=NONE}]
               @
               ({label=SOME loop_label, o=NONE, arg1=NONE, arg2=NONE, result=NONE} ::
               bool_exp) @
               [{label=NONE, o=SOME Goto, arg1=exp_val, arg2=NONE, result = SOME
               (Target end_label)}]
             end
         | (AST.Break, {continue_label=_, break_label=SOME bl}) =>
             [{label=NONE, o=SOME Goto, arg1=NONE, arg2=NONE, result = SOME
             (Target bl)}]
         | (AST.Break, {continue_label=_, break_label=NONE}) =>
             raise Fail "Break outside loop"
         | (AST.Continue, {continue_label=SOME cl, break_label=_}) =>
             [{label=NONE, o=SOME Goto, arg1=NONE, arg2=NONE, result = SOME
             (Target cl)}]
         | (AST.Continue, {continue_label=NONE, break_label=_}) =>
             raise Fail "Continue outside loop"
         | (AST.Exp (SOME exp), {continue_label=_, break_label=_}) => con_exp exp
         | (AST.Exp NONE , {continue_label=_, break_label=_}) => []
  in
    []
  end

end
