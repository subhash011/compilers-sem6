structure PrintAST =
struct
    open Tiger;
    fun name s = Symbol.name s
    and print_str s = TextIO.output (TextIO.stdOut, s)
    and print_strs [] = []
        | print_strs (x::xs) = (print_str x) :: (print_strs xs)

    and get_strs_exp (Array a) = let
                                    val {type_, length, init} = a
                                in
                                    ["Array(type_ = ", name type_, ", length = "] @ (get_strs_exp length) @ [", init = "] @ (get_strs_exp init) @ [")"]
                                end
        | get_strs_exp (Int i) = (["Int(", Int.toString i ,")"])
        | get_strs_exp (String s) = (["String(", s ,")"])
        | get_strs_exp (Record r) = let
                                        val {type_, init} = r
                                        val init_terms = get_records_init init
                                    in
                                        ["Record(type_ = ", name type_, ", init = "] @ init_terms @ [")"]
                                    end
        | get_strs_exp (New t) = ["New(", name t, ")"]
        | get_strs_exp (Lval l) = get_strs_lval l
        | get_strs_exp (FunctionCall f) = let
                                                val {id, args} = f
                                                val args_val = ["["] @  get_strs_exps args @ ["]"]
                                            in
                                                ["FunctionCall(id = ", name id, ", args = "] @ args_val
                                            end
        | get_strs_exp (MethodCall f) = let
                                            val {object, id, args} = f
                                            val obj_val = get_strs_lval object
                                            val args_val = ["["] @  get_strs_exps args @ ["]"]
                                        in
                                            ["MethodCall(object = "] @ obj_val @ [", id = ", name id, ", args = "] @ args_val
                                        end
        | get_strs_exp (Negate n) = (["Negate("] @ (get_strs_exp n) @ [")"])
        | get_strs_exp (Exps e) = ["["] @  get_strs_exps e @ ["]"]
        | get_strs_exp (Assignment a) = let
                                            val {object, exp} = a
                                            val obj_val = get_strs_lval object
                                        in
                                            ["Assignment(object = "] @ obj_val @ [", exp = "] @ get_strs_exp exp
                                        end
        | get_strs_exp (IfElse ie) = let
                                            val {cond, succ, fail} = ie
                                            val cond_val = get_strs_exp cond
                                            val succ_val = get_strs_exp succ
                                        in
                                            case fail of
                                            SOME t => ["IfElse(cond = "] @ cond_val @ [", succ = "] @ succ_val @ [", fail = "] @ (get_strs_exp t) @ [")"]
                                            | NONE => ["IfElse(cond = "] @ cond_val @ [", succ = "] @ succ_val @ [", fail = "] @ [")"]
                                        end
        | get_strs_exp (While w) = let
                                            val {cond, body} = w
                                            val cond_val = get_strs_exp cond
                                            val body_val = get_strs_exp body
                                        in
                                            ["While(cond = "] @ cond_val @ [", body = "] @ body_val @ [")"]
                                        end
        | get_strs_exp (For f) = let
                                            val {init, exit_cond, body} = f
                                            val {var, exp} = init
                                            val exp = get_strs_exp exp
                                            val exit_val = get_strs_exp exit_cond
                                            val body_val = get_strs_exp body
                                        in
                                            ["For(init = {var = ", name var, ", exp = "] @ exp @ ["}, exit_cond = "] @ exit_val @ [", body = "] @ body_val @ [")"]
                                        end
        | get_strs_exp Break = ["Break"]
        | get_strs_exp (Let l) = let
                                    val {decs, body} = l
                                    val decs_val = ["["] @  get_strs_decs decs @ ["]"]
                                    val body_val = ["["] @  get_strs_exps body @ ["]"]
                                in
                                    ["Let(decs = "] @ decs_val @ [", body = "] @ body_val @ [")"]
                                end
        | get_strs_exp (Op operation) = let
                                    val {left, oper, right} = operation
                                    val opval = (get_op oper)
                                in
                                    ["Op(left = "] @ (get_strs_exp left) @ [", oper = "] @ opval @ [", right = "] @ (get_strs_exp right)
                                end
        | get_strs_exp (Nil) = ["Nil"]
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

    and get_strs_exps [] = []
        | get_strs_exps (x::xs) = (get_strs_exp x) @ (get_strs_exps xs)

    and get_strs_lval (Variable v) = ["Variable(", name v, ")"]
        | get_strs_lval (Reference r) = let
                                            val {object, var} = r
                                            val obj_val = get_strs_lval object
                                        in
                                            ["Reference(object = "] @ obj_val @ [", var = ", name var]
                                        end
        | get_strs_lval (ArrayAccess a) = let
                                                val {object, index} = a
                                                val obj_val = get_strs_lval object
                                                val index_val = get_strs_exp index
                                            in
                                                ["ArrayAccess(object = "] @ obj_val @ [", index = "] @ index_val
                                            end

    and get_record_init r = let
                                val {var, value} = r
                            in
                                ["{var = ", (name var), ", value = "] @ get_strs_exp value @ ["}, "]
                            end

    and get_records_init [] = []
        | get_records_init (x::xs) = (get_record_init x) @ (get_records_init xs)

    and get_strs_tyfield t = let
                                    val {var, type_} = t;
                                in
                                    ["var = ", name var, ", type_ = ", (name type_)]
                                end

    and get_strs_classfield (AttrDec a) = let
                                                val {id, type_, init} = a
                                                val exp = get_strs_exp init
                                            in
                                                case type_ of
                                                SOME t => (["AttrDec(id = ", name id, ", type_ = ", name t, ", init = "] @ exp  @ [")"])
                                                | NONE => (["AttrDec(id = ", name id, ", init = "] @ exp @ [")"])
                                            end
        | get_strs_classfield (MethodDec m) = get_strs_fundectype "MethodDec" m

    and get_strs_fundectype what f = let
                                    val {id, args, ret, body} = f
                                    val tyfields = ["{"] @ get_strs_tyfields args @ ["}"]
                                    val exp = get_strs_exp body
                                in
                                    case ret of
                                    SOME t => ([what, "(id = ", name id, ", args = "] @ tyfields @ [", ret = ", name t] @ [", body = "] @ exp @ [")"])
                                    | NONE => ([what, "(id = ", name id, ", args = "] @ tyfields @ [", body = "] @ exp @ [")"])
                                end

    and get_strs_dec (VarDec v) = let
                                    val {id, type_, init} = v;
                                    val exp = get_strs_exp init
                                in
                                case type_ of
                                SOME t => (["VarDec(id = ", name id, ", type_ = ", name t, ", init = "] @ exp @ [")"])
                                | NONE => (["VarDec(id = ", name id, ", init = "] @ exp @ [")"])
                                end
        | get_strs_dec (ClassDec c) = let
                                            val {id, extends, classfields} = c
                                            val cfs = ["["] @  get_strs_classfields classfields @ ["]"]
                                        in
                                            case extends of
                                            SOME e => (["ClassDec(id = ", name id, ", extends = ", name id, ", classfields = "] @ cfs @ [")"])
                                            | NONE => (["ClassDec(id = ", name id, ", classfields = "] @ cfs @ [")"])
                                        end

        | get_strs_dec (TypeDec t) = let val {id, type_} = t in ["TypeDec(id = ", name id, ", type_ = "] @ get_strs_type type_ @ [")"] end
        | get_strs_dec (FunDec f) = get_strs_fundectype "FunDec" f
        | get_strs_dec (PrimitiveDec p) = get_strs_fundectype "PrimitiveDec" p

    and get_strs_type (TypeAlias t) = ["TypeAlias(", name t,")"]
        | get_strs_type (RecordType r) = let
                                            val tyfields = ["{"] @ get_strs_tyfields r @ ["}"]
                                        in
                                            ["RecordType(["] @ tyfields @ ["])"]
                                        end
        | get_strs_type (ArrayType a) = ["ArrayType(", name a, ")"]
        | get_strs_type (ClassType c) = let
                                            val {extends, classfields} = c
                                            val cfs = ["["] @  get_strs_classfields classfields @ ["]"]
                                        in
                                            case extends of
                                            SOME e => (["ClassType(extends = ", name e, ", classfields = "] @ cfs)
                                            | NONE => (["ClassType(classfields = "] @ cfs)
                                        end

    and get_strs_tyfields [] = []
        | get_strs_tyfields (x::xs) = (case xs of
                                            [] => (get_strs_tyfield x) @ (get_strs_tyfields xs)
                                            | _ => (get_strs_tyfield x) @ ["}, {"] @ (get_strs_tyfields xs)
                                        )

    and get_strs_classfields [] = []
        | get_strs_classfields (x::xs) = (case xs of
                                            [] => (get_strs_classfield x) @ (get_strs_classfields xs)
                                            | _ => (get_strs_classfield x) @ [", "] @  (get_strs_classfields xs)
                                            )

    and get_strs_decs [] = []
        | get_strs_decs (x::xs) = (case xs of
                                        [] => (get_strs_dec x) @  (get_strs_decs xs)
                                        | _ => (get_strs_dec x) @ [", "] @  (get_strs_decs xs)
                                    )

    and get_strs_ast (Decs decs) = (["Decs(["] @ get_strs_decs decs @ ["])"] @ ["\n"])
        | get_strs_ast (Expr exp) = (["Exp("] @ get_strs_exp exp @ [")"] @ ["\n"])

    and print_ast a = print_strs (get_strs_ast a);

end