structure PrintAST :
sig
   val print : Tiger.ast -> unit
end = struct
    open Tiger;
    fun sym_name s = Symbol.name s
    and print_str s = TextIO.output (TextIO.stdOut, s)
    and strs [] = ""
        | strs (x::xs) = x ^ (strs xs)

    and strs_exp (Array a) =    let
                                    val {type_, length, init} = a
                                in
                                    ["Array({type_ = ", sym_name type_, ", length = "] @ (strs_exp length) @ [", init = "] @ (strs_exp init) @ ["})"]
                                end
        | strs_exp (Int i) = (["Int(", Int.toString i ,")"])
        | strs_exp (String s) = (["String(\"", s ,"\")"])
        | strs_exp (Record r) = let
                                    val {type_, init} = r
                                    val init_terms = get_records_init init
                                in
                                    ["Record({type_ = ", sym_name type_, ", init = "] @ init_terms @ ["})"]
                                end
        | strs_exp (New t) = ["New(", sym_name t, ")"]
        | strs_exp (LvalExp l) =  ["LvalExp("] @ strs_lval l @ [")"]
        | strs_exp (FunctionCall f) =   let
                                            val {name, args} = f
                                            val args_val = ["["] @  strs_exps args @ ["]"]
                                        in
                                            ["FunctionCall({name = ", sym_name name, ", args = "] @ args_val @ ["})"]
                                        end
        | strs_exp (MethodCall f) =     let
                                            val {object, name, args} = f
                                            val obj_val = strs_lval object
                                            val args_val = ["["] @  strs_exps args @ ["]"]
                                        in
                                            ["MethodCall({object = "] @ obj_val @ [", name = ", sym_name name, ", args = "] @ args_val @ ["})"]
                                        end
        | strs_exp (Negate n) = (["Negate("] @ (strs_exp n) @ [")"])
        | strs_exp (Exps e) = ["Exps(["] @  strs_exps e @ ["])"]
        | strs_exp (Assignment a) =     let
                                            val {object, exp} = a
                                            val obj_val = strs_lval object
                                        in
                                            ["Assignment({object = "] @ obj_val @ [", exp = "] @ strs_exp exp @ ["})"]
                                        end
        | strs_exp (IfElse ie) =    let
                                        val {cond, succ, fail} = ie
                                        val cond_val = strs_exp cond
                                        val succ_val = strs_exp succ
                                    in
                                        case fail of
                                        SOME t => ["IfElse({cond = "] @ cond_val @ [", succ = "] @ succ_val @ [", fail = "] @ (strs_exp t) @ ["})"]
                                        | NONE => ["IfElse({cond = "] @ cond_val @ [", succ = "] @ succ_val @ [", fail = NONE"] @ ["})"]
                                    end
        | strs_exp (While w) =  let
                                    val {cond, body} = w
                                    val cond_val = strs_exp cond
                                    val body_val = strs_exp body
                                in
                                    ["While({cond = "] @ cond_val @ [", body = "] @ body_val @ ["})"]
                                end
        | strs_exp (For f) =    let
                                    val {init, exit_cond, body} = f
                                    val {name, exp} = init
                                    val exp = strs_exp exp
                                    val exit_val = strs_exp exit_cond
                                    val body_val = strs_exp body
                                in
                                    ["For({init = {name = ", sym_name name, ", exp = "] @ exp @ ["}, exit_cond = "] @ exit_val @ [", body = "] @ body_val @ ["})"]
                                end
        | strs_exp Break = ["Break"]
        | strs_exp (Let l) =    let
                                    val {decs, body} = l
                                    val decs_val = ["["] @  strs_decs decs @ ["]"]
                                    val body_val = ["["] @  strs_exps body @ ["]"]
                                in
                                    ["Let({decs = "] @ decs_val @ [", body = "] @ body_val @ ["})"]
                                end
        | strs_exp (Op operation) = let
                                        val {left, oper, right} = operation
                                        val opval = (get_op oper)
                                    in
                                        ["Op({left = "] @ (strs_exp left) @ [", oper = "] @ opval @ [", right = "] @ (strs_exp right) @ ["})"]
                                    end
        | strs_exp (Nil) = ["Nil"]

    and get_op Plus = ["Plus"]
        | get_op Minus = ["Minus"]
        | get_op Mul = ["Mul"]
        | get_op Div = ["Div"]
        | get_op Eq = ["Eq"]
        | get_op Ne = ["Ne"]
        | get_op Gt = ["Gt"]
        | get_op Lt = ["Lt"]
        | get_op Ge = ["Ge"]
        | get_op Le = ["Le"]
        | get_op And = ["And"]
        | get_op Or = ["Or"]

    and strs_lval (Variable v) = ["Variable(", sym_name v, ")"]
        | strs_lval (Reference r) = let
                                        val {object, name} = r
                                        val obj_val = ["LvalExp("] @ strs_lval object @ [")"]
                                    in
                                        ["Reference({object = "] @ obj_val @ [", name = ", sym_name name] @ ["})"]
                                    end
        | strs_lval (ArrayAccess a) =   let
                                            val {object, index} = a
                                            val obj_val = ["LvalExp("] @ strs_lval object @ [")"]
                                            val index_val = strs_exp index
                                        in
                                            ["ArrayAccess({object = "] @ obj_val @ [", index = "] @ index_val @ ["})"]
                                        end

    and get_record_init r = let
                                val {name, value} = r
                            in
                                ["{name = ", (sym_name name), ", value = "] @ strs_exp value @ ["}, "]
                            end

    and get_records_init [] = []
        | get_records_init (x::xs) = (get_record_init x) @ (get_records_init xs)

    and strs_tyfield t =    let
                                val {name, type_} = t;
                            in
                                ["{", "name = ", sym_name name, ", type_ = ", (sym_name type_), "}"]
                            end

    and strs_classfield (AttrDec a) =   let
                                            val {name, type_, init} = a
                                            val exp = strs_exp init
                                        in
                                            case type_ of
                                            SOME t => (["AttrDec({name = ", sym_name name, ", type_ = ", sym_name t, ", init = "] @ exp  @ ["})"])
                                            | NONE => (["AttrDec({name = ", sym_name name, ", init = "] @ exp @ ["})"])
                                        end
        | strs_classfield (MethodDec m) = strs_fundectype "MethodDec" m

    and strs_fundectype what f =    let
                                        val {name, args, ret, body} = f
                                        val tyfields = ["["] @ strs_tyfields args @ ["]"]
                                        val exp = strs_exp body
                                    in
                                        case ret of
                                        SOME t => ([what, "({name = ", sym_name name, ", args = "] @ tyfields @ [", ret = ", sym_name t] @ [", body = "] @ exp @ ["})"])
                                        | NONE => ([what, "({name = ", sym_name name, ", args = "] @ tyfields @ [", body = "] @ exp @ ["})"])
                                    end

    and strs_dec (VarDec v) =   let
                                    val {name, type_, init} = v;
                                    val exp = strs_exp init
                                in
                                    case type_ of
                                    SOME t => (["VarDec({name = ", sym_name name, ", type_ = ", sym_name t, ", init = "] @ exp @ ["})"])
                                    | NONE => (["VarDec({name = ", sym_name name, ", init = "] @ exp @ ["})"])
                                end
        | strs_dec (ClassDec c) =   let
                                        val {name, extends, classfields} = c
                                        val cfs = ["["] @  strs_classfields classfields @ ["]"]
                                    in
                                        case extends of
                                        SOME e => (["ClassDec({name = ", sym_name name, ", extends = ", sym_name e, ", classfields = "] @ cfs @ ["})"])
                                        | NONE => (["ClassDec({name = ", sym_name name, ", classfields = "] @ cfs @ ["})"])
                                    end

        | strs_dec (TypeDec t) =    let
                                        val {name, type_} = t
                                    in
                                        ["TypeDec({name = ", sym_name name, ", type_ = "] @ strs_type type_ @ ["})"]
                                    end

        | strs_dec (FunDec f) = strs_fundectype "FunDec" f
        | strs_dec (PrimitiveDec p) = let
                                            val {name, args, ret} = p
                                            val tyfields = ["["] @ strs_tyfields args @ ["]"]
                                        in
                                            case ret of
                                            SOME t => (["PrimitiveDec({name = ", sym_name name, ", args = "] @ tyfields @ [", ret = ", sym_name t] @ ["})"])
                                            | NONE => (["PrimitiveDec({name = ", sym_name name, ", args = "] @ tyfields @ ["})"])
                                        end

    and strs_type (TypeAlias t) = ["TypeAlias(", sym_name t,")"]
        | strs_type (RecordType r) =    let
                                            val tyfields = ["["] @ strs_tyfields r @ ["]"]
                                        in
                                            ["RecordType("] @ tyfields @ [")"]
                                        end
        | strs_type (ArrayType a) = ["ArrayType(", sym_name a, ")"]
        | strs_type (ClassType c) =     let
                                            val {extends, classfields} = c
                                            val cfs = ["["] @  strs_classfields classfields @ ["]"]
                                        in
                                            case extends of
                                            SOME e => (["ClassType({extends = ", sym_name e, ", classfields = "] @ cfs @ ["})"])
                                            | NONE => (["ClassType({classfields = "] @ cfs @ ["})"])
                                        end

    and strs_tyfields [] = []
        | strs_tyfields (x::xs) =   (case xs of
                                        [] => (strs_tyfield x) @ (strs_tyfields xs)
                                        | _ => (strs_tyfield x) @ [", "] @ (strs_tyfields xs)
                                    )

    and strs_classfields [] = []
        | strs_classfields (x::xs) =    (case xs of
                                            [] => (strs_classfield x) @ (strs_classfields xs)
                                            | _ => (strs_classfield x) @ [", "] @  (strs_classfields xs)
                                        )
    and strs_exps [] = []
        | strs_exps (x::xs) =   (case xs of
                                    [] => (strs_exp x) @ (strs_exps xs)
                                    | _ => (strs_exp x) @ [", "] @ (strs_exps xs)
                                )

    and strs_decs [] = []
        | strs_decs (x::xs) =   (case xs of
                                    [] => (strs_dec x) @  (strs_decs xs)
                                    | _ => (strs_dec x) @ [", "] @  (strs_decs xs)
                                )

    and strs_ast (Expr exp) = (["Expr("] @ strs_exp exp @ [")"] @ ["\n"])
        | strs_ast (Decs decs) = (["Decs(["] @ strs_decs decs @ ["])"] @ ["\n"])

    and print a = print_str (strs (strs_ast a));
end
