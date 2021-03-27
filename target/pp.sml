structure PP :
sig
   val compile : Tiger.ast -> unit
end = struct
    open Tiger;
    fun sym_name s = Symbol.name s
    and print_str s = TextIO.output (TextIO.stdOut, s)
    and strs [] = ""
        | strs (x::xs) = x ^ (strs xs)

    and strs_exp (Array a) =    let
                                    val {type_, length, init} = a
                                in
                                    [sym_name type_, "["]  @ (strs_exp length) @ ["]", " of "] @ (strs_exp init)
                                end
        | strs_exp (Int i) = ([Int.toString i])
        | strs_exp (String s) = (["\"", s ,"\""])
        | strs_exp (Record r) = let
                                    val {type_, init} = r
                                    val init_terms = get_records_init init
                                in
                                    [sym_name type_, " {"] @ init_terms @ ["}"]
                                end
        | strs_exp (New t) = ["new ", sym_name t]
        | strs_exp (LvalExp l) =  strs_lval l
        | strs_exp (FunctionCall f) =   let
                                            val {name, args} = f
                                            val args_val = functionCallArgs args
                                        in
                                            [sym_name name, "("] @ args_val @ [")"]
                                        end
        | strs_exp (MethodCall f) =     let
                                            val {object, name, args} = f
                                            val obj_val = strs_lval object
                                            val args_val = functionCallArgs args
                                        in
                                            obj_val @ [".", sym_name name] @ ["("] @ args_val @ [")"]
                                        end
        | strs_exp (Negate n) = ["-"] @ (strs_exp n)
        | strs_exp (Exps e) = ["("] @  strs_exps e @ [")"]
        | strs_exp (Assignment a) =     let
                                            val {object, exp} = a
                                            val obj_val = strs_lval object
                                        in
                                            obj_val @ [" := "] @ strs_exp exp
                                        end
        | strs_exp (IfElse ie) =    let
                                        val {cond, succ, fail} = ie
                                        val cond_val = strs_exp cond
                                        val succ_val = strs_exp succ
                                    in
                                        case fail of
                                        SOME t => ["if ("] @ cond_val @ [") then "] @ succ_val @ [" else "] @ (strs_exp t)
                                        | NONE => ["if ("] @ cond_val @ [") then "] @ succ_val
                                    end
        | strs_exp (While w) =  let
                                    val {cond, body} = w
                                    val cond_val = strs_exp cond
                                    val body_val = strs_exp body
                                in
                                    ["while"] @ cond_val @ [" do\n"] @ body_val
                                end
        | strs_exp (For f) =    let
                                    val {init, exit_cond, body} = f
                                    val {name, exp} = init
                                    val exp = strs_exp exp
                                    val exit_val = strs_exp exit_cond
                                    val body_val = strs_exp body
                                in
                                    ["for(", sym_name name, " := "] @ exp @ [" to "] @ exit_val @ [" do\n"] @ body_val
                                end
        | strs_exp Break = ["Break"]
        | strs_exp (Let l) =    let
                                    val {decs, body} = l
                                    val decs_val = strs_decs decs
                                    val body_val = strs_exps body
                                in
                                    ["let\n"] @ decs_val @ ["\nin\n"] @ body_val @ ["\nend"]
                                end
        | strs_exp (Op operation) = let
                                        val {left, oper, right} = operation
                                        val opval = (get_op oper)
                                    in
                                        (strs_exp left) @ opval @ (strs_exp right)
                                    end
        | strs_exp Nil = ["nil"]

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

    and strs_lval (Variable v) = [sym_name v]
        | strs_lval (Reference r) = let
                                        val {object, name} = r
                                        val obj_val = strs_lval object
                                    in
                                        obj_val @ [".", sym_name name]
                                    end
        | strs_lval (ArrayAccess a) =   let
                                            val {object, index} = a
                                            val obj_val = strs_lval object
                                            val index_val = strs_exp index
                                        in
                                            obj_val @ ["["] @ index_val @ ["]"]
                                        end

    and get_record_init r = let
                                val {name, value} = r
                            in
                                [(sym_name name), " = "] @ strs_exp value
                            end

    and get_records_init [] = []
        | get_records_init (x::xs) = (case xs of
                                        [] => (get_record_init x)
                                        | _ => (get_record_init x) @ [", "] @ (get_records_init xs)
                                    )

    and strs_tyfield t =    let
                                val {name, type_} = t;
                            in
                                [sym_name name, ": ", (sym_name type_)]
                            end

    and strs_classfield (AttrDec a) =   let
                                            val {name, type_, init} = a
                                            val exp = strs_exp init
                                        in
                                            case type_ of
                                            SOME t => ["var ", sym_name name, " := ", sym_name t, " = "] @ exp
                                            | NONE => ["var ", sym_name name, " := "] @ exp
                                        end
        | strs_classfield (MethodDec m) = strs_fundectype "method" m

    and strs_fundectype what f =    let
                                        val {name, args, ret, body} = f
                                        val tyfields = strs_tyfields args
                                        val exp = strs_exp body
                                    in
                                        case ret of
                                        SOME t => ([what, " ", sym_name name, "("] @ tyfields @ [")"] @ [": ", sym_name t] @ [" = "] @ exp)
                                        | NONE => ([what, " ", sym_name name, "("] @ tyfields @ [")"] @ [" = "] @ exp)
                                    end

    and strs_dec (VarDec v) =   let
                                    val {name, type_, init} = v;
                                    val exp = strs_exp init
                                in
                                    case type_ of
                                    SOME t => ["var ", sym_name name, " := ", sym_name t, " = "] @ exp
                                    | NONE => ["var ", sym_name name, " := "] @ exp
                                end
        | strs_dec (ClassDec c) =   let
                                        val {name, extends, classfields} = c
                                        val cfs = ["["] @  strs_classfields classfields @ ["]"]
                                    in
                                        case extends of
                                        SOME e => (["class ", sym_name name, ", extends ", sym_name e, " {\n"] @ cfs @ ["\n}"])
                                        | NONE => (["class ", sym_name name, " {\n"] @ cfs @ ["\n}"])
                                    end

        | strs_dec (TypeDec t) =    let
                                        val {name, type_} = t
                                    in
                                        ["type ", sym_name name, " = "] @ strs_type type_
                                    end

        | strs_dec (FunDec f) = strs_fundectype "function" f
        | strs_dec (PrimitiveDec p) = let
                                            val {name, args, ret} = p
                                            val tyfields = ["["] @ strs_tyfields args @ ["]"]
                                        in
                                            case ret of
                                            SOME t => ["primitive ", sym_name name, "("] @ tyfields @ ["): ", sym_name t]
                                            | NONE => ["primitive ", sym_name name, "("] @ tyfields @ [")"]
                                        end

    and strs_type (TypeAlias t) = [sym_name t]
        | strs_type (RecordType r) =    let
                                            val tyfields = ["{"] @ strs_tyfields r @ ["}"]
                                        in
                                            tyfields
                                        end
        | strs_type (ArrayType a) = ["array of ", sym_name a]
        | strs_type (ClassType c) =     let
                                            val {extends, classfields} = c
                                            val cfs = strs_classfields classfields
                                        in
                                            case extends of
                                            SOME e => ["class extends ", sym_name e, " {\n"] @ cfs @ ["\n}"]
                                            | NONE => ["class {\n"] @ cfs @ ["\n}"]
                                        end

    and strs_tyfields [] = []
        | strs_tyfields (x::xs) =   (case xs of
                                        [] => (strs_tyfield x) @ (strs_tyfields xs)
                                        | _ => (strs_tyfield x) @ [", "] @ (strs_tyfields xs)
                                    )

    and strs_classfields [] = []
        | strs_classfields (x::xs) =    (case xs of
                                            [] => (strs_classfield x) @ (strs_classfields xs)
                                            | _ => (strs_classfield x) @ ["\n"] @  (strs_classfields xs)
                                        )

    and functionCallArgs [] = []
        | functionCallArgs [x] = (strs_exp x)
        | functionCallArgs (x::xs) = (strs_exp x) @ [", "] @ (strs_exps xs)

    and strs_exps [] = []
        | strs_exps (x::xs) =   (case xs of
                                    [] => (strs_exp x)
                                    | _ => (strs_exp x) @ [";\n"] @ (strs_exps xs)
                                )

    and strs_decs [] = []
        | strs_decs (x::xs) =   (case xs of
                                    [] => (strs_dec x)
                                    | _ => (strs_dec x) @ ["\n"] @  (strs_decs xs)
                                )

    and strs_ast (Expr exp) = strs_exp exp
        | strs_ast (Decs decs) = strs_decs decs

    and compile a = (print_str (strs (strs_ast a)); print("\n"));
end
