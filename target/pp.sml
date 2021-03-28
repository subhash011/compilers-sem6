structure PP :
sig
   val compile : Tiger.ast -> unit
end = struct
    open Tiger;
    fun sym_name s = Symbol.name s
    and spaces 0 = [""]
        | spaces i = [" "] @ (spaces (i - 1))
    and print_str s = TextIO.output (TextIO.stdOut, s)

    and strs_exp (Array a) ind =    let
                                        val {type_, length, init} = a
                                    in
                                        spaces ind @ [sym_name type_, "["]  @ (strs_exp length 0) @ ["]", " of "] @ (strs_exp init 0)
                                    end
        | strs_exp (Int i) ind = spaces ind @ ([Int.toString i])
        | strs_exp (String s) ind = spaces ind @  (["\"", s ,"\""])
        | strs_exp (Record r) ind = let
                                    val {type_, init} = r
                                    val init_terms = get_records_init init
                                in
                                     spaces ind @ [sym_name type_, " {"] @ init_terms @ ["}"]
                                end
        | strs_exp (New t) ind = ["new ", sym_name t]
        | strs_exp (LvalExp l) ind =  strs_lval l ind
        | strs_exp (FunctionCall f) ind =   let
                                            val {name, args} = f
                                            val args_val = functionCallArgs args 0
                                        in
                                            spaces ind @ [sym_name name, "("] @ args_val @ [")"]
                                        end
        | strs_exp (MethodCall f) ind =     let
                                            val {object, name, args} = f
                                            val obj_val = strs_lval object 0
                                            val args_val = functionCallArgs args 0
                                        in
                                            spaces ind @ obj_val @ [".", sym_name name] @ ["("] @ args_val @ [")"]
                                        end
        | strs_exp (Negate n) ind = spaces ind @ ["-"] @ (strs_exp n ind)
        | strs_exp (Exps e) ind =   strs_exps e (ind)
        | strs_exp (Assignment a) ind =     let
                                            val {object, exp} = a
                                            val obj_val = strs_lval object 0
                                        in
                                            spaces ind @ obj_val @ [" := "] @ strs_exp exp 0
                                        end
        | strs_exp (IfElse ie) ind =    let
                                            val {cond, succ, fail} = ie
                                            val cond_val = strs_exp cond 0
                                            val succ_val = strs_exp succ (ind + 4)
                                        in
                                            case fail of
                                            SOME t =>   spaces ind @ ["if ("] @ cond_val @ [")\n"]
                                                        @ spaces ind @ ["then\n"]
                                                        @ succ_val @ ["\n"]
                                                        @ spaces ind @ ["else\n"]
                                                        @ (strs_exp t (ind + 4))
                                            | NONE =>   spaces ind @ ["if ("] @ cond_val @ [")\n"]
                                                        @ spaces ind @ ["then\n"]
                                                        @ succ_val
                                        end
        | strs_exp (While w) ind =  let
                                    val {cond, body} = w
                                    val cond_val = cond_expr cond
                                    val body_val = strs_exp body (ind + 4)
                                in
                                    spaces ind @ ["while ("] @ cond_val @ [") do (\n"]
                                    @ body_val @ ["\n"]
                                    @ spaces ind @ [")"]
                                end
        | strs_exp (For f) ind =    let
                                        val {init, exit_cond, body} = f
                                        val {name, exp} = init
                                        val exp = strs_exp exp 0
                                        val exit_val = strs_exp exit_cond 0
                                        val body_val = strs_exp body (ind + 4)
                                    in
                                        spaces ind @ ["for (", sym_name name, " := "] @ exp @ [" to "] @ exit_val @ [" do (\n"]
                                        @ body_val @ ["\n"]
                                        @ spaces ind @ [")"]
                                    end
        | strs_exp Break ind = ["Break"]
        | strs_exp (Let l) ind =    let
                                    val {decs, body} = l
                                    val decs_val = strs_decs decs (ind + 4)
                                    val body_val = strs_exps body (ind + 4)
                                in
                                    spaces ind @ ["let\n"]
                                    @ decs_val @ ["\n"]
                                    @ spaces ind @ ["in\n"]
                                    @ body_val @ ["\n"]
                                    @ spaces ind @ ["end"]
                                end
        | strs_exp (Op operation) ind = let
                                        val {left, oper, right} = operation
                                        val opval = (get_op oper)
                                    in
                                         spaces ind @ (strs_exp left 0) @ opval @ (strs_exp right 0)
                                    end
        | strs_exp Nil ind = spaces ind @ ["nil"]

    and get_op Plus = [" + "]
        | get_op Minus = [" - "]
        | get_op Mul = [" * "]
        | get_op Div = [" / "]
        | get_op Eq = [" = "]
        | get_op Ne = [" <> "]
        | get_op Gt = [" > "]
        | get_op Lt = [" < "]
        | get_op Ge = [" >= "]
        | get_op Le = [" <= "]
        | get_op And = [" & "]
        | get_op Or = [" | "]

    and strs_lval (Variable v) ind = spaces ind @ [sym_name v]
        | strs_lval (Reference r) ind = let
                                        val {object, name} = r
                                        val obj_val = strs_lval object ind
                                    in
                                        obj_val @ [".", sym_name name]
                                    end
        | strs_lval (ArrayAccess a) ind =   let
                                            val {object, index} = a
                                            val obj_val = strs_lval object ind
                                            val index_val = strs_exp index 0
                                        in
                                            obj_val @ ["["] @ index_val @ ["]"]
                                        end

    and get_record_init r = let
                                val {name, value} = r
                            in
                                [(sym_name name), " = "] @ strs_exp value 0
                            end

    and get_records_init [] = []
        | get_records_init (x::xs) = (case xs of
                                        [] => (get_record_init x)
                                        | _ => (get_record_init x) @ [", "] @ (get_records_init xs)
                                    )

    and strs_tyfield t ind =    let
                                val {name, type_} = t;
                            in
                                [sym_name name, ": ", (sym_name type_)]
                            end

    and strs_classfield (AttrDec a) ind =   let
                                            val {name, type_, init} = a
                                            val exp = strs_exp init ind
                                        in
                                            case type_ of
                                            SOME t => spaces ind @ ["var ", sym_name name, " := ", sym_name t, " = "] @ exp
                                            | NONE => spaces ind @ ["var ", sym_name name, " := "] @ exp
                                        end
        | strs_classfield (MethodDec m) ind = strs_fundectype "method" m ind

    and strs_fundectype what f ind =    let
                                        val {name, args, ret, body} = f
                                        val tyfields = strs_tyfields args 0
                                        val exp = strs_exp body (ind + 4)
                                    in
                                        case ret of
                                        SOME t =>   spaces ind @ [what, " ", sym_name name, "("] @ tyfields @ [")"] @ [": ", sym_name t] @ [" = (\n"]
                                                    @ exp @ ["\n"]
                                                    @ spaces ind @ [")"]
                                        | NONE =>   spaces ind @ [what, " ", sym_name name, "("] @ tyfields @ [")"] @ [" = (\n"]
                                                    @ exp @ ["\n"]
                                                    @ spaces ind @ [")"]
                                    end

    and strs_dec (VarDec v) ind =   let
                                    val {name, type_, init} = v;
                                    val exp = strs_exp init 0
                                in
                                    case type_ of
                                    SOME t => spaces ind @ ["var ", sym_name name, ": ", sym_name t, " = "] @ exp
                                    | NONE => spaces ind @ ["var ", sym_name name, " := "] @ exp
                                end
        | strs_dec (ClassDec c) ind =   let
                                        val {name, extends, classfields} = c
                                        val cfs = ["["] @  strs_classfields classfields ind @ ["]"]
                                    in
                                        case extends of
                                        SOME e => (["class ", sym_name name, ", extends ", sym_name e, " {\n"] @ cfs @ ["\n}"])
                                        | NONE => (["class ", sym_name name, " {\n"] @ cfs @ ["\n}"])
                                    end

        | strs_dec (TypeDec t) ind =    let
                                        val {name, type_} = t
                                    in
                                        spaces ind @ ["type ", sym_name name, " = "] @ strs_type type_ ind
                                    end

        | strs_dec (FunDec f) ind = strs_fundectype "function" f ind
        | strs_dec (PrimitiveDec p) ind = let
                                            val {name, args, ret} = p
                                            val tyfields = ["["] @ strs_tyfields args ind @ ["]"]
                                        in
                                            case ret of
                                            SOME t => ["primitive ", sym_name name, "("] @ tyfields @ ["): ", sym_name t]
                                            | NONE => ["primitive ", sym_name name, "("] @ tyfields @ [")"]
                                        end

    and strs_type (TypeAlias t) ind = [sym_name t]
        | strs_type (RecordType r) ind =    let
                                            val tyfields = ["{"] @ strs_tyfields r ind @ ["}"]
                                        in
                                            tyfields
                                        end
        | strs_type (ArrayType a) ind = ["array of ", sym_name a]
        | strs_type (ClassType c) ind =     let
                                            val {extends, classfields} = c
                                            val cfs = strs_classfields classfields (ind + 4)
                                        in
                                            case extends of
                                            SOME e => ["class extends ", sym_name e, " {\n"] @ cfs @ ["\n"] @ spaces ind @ ["}"]
                                            | NONE => ["class {\n"] @ cfs @ ["\n}"]
                                        end

    and strs_tyfields [] ind = []
        | strs_tyfields (x::xs) ind =   (case xs of
                                        [] => (strs_tyfield x ind) @ (strs_tyfields xs ind)
                                        | _ => (strs_tyfield x ind) @ [", "] @ (strs_tyfields xs ind)
                                    )

    and strs_classfields [] ind = []
        | strs_classfields (x::xs) ind =    (case xs of
                                            [] => (strs_classfield x ind) @ (strs_classfields xs ind)
                                            | _ => (strs_classfield x ind) @ ["\n"] @ (strs_classfields xs ind)
                                        )

    and functionCallArgs [] ind = []
        | functionCallArgs [x] ind = (strs_exp x ind)
        | functionCallArgs (x::xs) ind = (strs_exp x ind) @ [", "] @ (functionCallArgs xs ind)

    and cond_exps [] = []
        | cond_exps (x::xs) = strs_exp x 0 @ cond_exps xs
    and cond_expr (Exps e) = cond_exps e
        | cond_expr e = strs_exp e 0

    and strs_exps [] ind = spaces ind @ ["(", ")"]
        | strs_exps (x::xs) ind =   (case xs of
                                    [] => (strs_exp x ind)
                                    | _ => (strs_exp x ind) @ [";\n"] @ (strs_exps xs ind)
                                )

    and strs_decs [] ind = []
        | strs_decs (x::xs) ind =   (case xs of
                                    [] => (strs_dec x ind)
                                    | _ => (strs_dec x ind) @ ["\n"] @ (strs_decs xs ind)
                                )

    and strs_ast (Expr exp) = strs_exp exp 0
        | strs_ast (Decs decs) = strs_decs decs 0

    and compile a = (print_str (String.concat (strs_ast a)); print("\n"));
end
