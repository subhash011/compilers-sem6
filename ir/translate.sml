signature TRANSLATE =
sig
    datatype exp =  Ex of Tree.exp
                    | Nx of Tree.stm
                    | Cx of Temp.label * Temp.label -> Tree.stm

    val unEx: exp -> Tree.exp
    val unNx: exp -> Tree.stm
    val unCx: exp -> Temp.label * Temp.label -> Tree.stm
end

structure Translate =
struct

    datatype exp =  Ex of Tree.exp
                    | Nx of Tree.stm
                    | Cx of Temp.label * Temp.label -> Tree.stm

    datatype Op = RelOp of Tree.relop
                | BinOp of Tree.binop

    structure A = Tiger
    structure T = Tree
    val nop = T.EXP (T.CONST 0)

    val table: Tree.exp Symbol.table ref = ref Symbol.empty

    exception TranslationError
    exception Unimplemented of string
    exception UndefinedVariable of string
    exception SyntaxError

    fun seq [x] = x
        | seq [x, y] = T.SEQ (x, y)
        | seq (x::xs) = T.SEQ (x, seq xs)
        | seq _ = raise TranslationError

    fun unEx (Ex e) = e
        | unEx (Nx s) = T.ESEQ (s, T.CONST 0)
        | unEx (Cx func) = 
            let
                val reg = Temp.newtemp() (* Stores the output of conditional exp. *)
                val succ = Temp.newlabel() (* Cond success label *)
                val fail = Temp.newlabel() (* Cond fail label *)
            in
                T.ESEQ (seq [
                                T.MOVE (T.TEMP reg, T.CONST 1), (* Move 1 into register. *)
                                func (succ, fail),
                                T.LABEL fail, (* If previous statement fails, it enters this block. *)
                                T.MOVE (T.TEMP reg, T.CONST 0), (* Set register to 0 since condition failed. *)
                                T.LABEL succ (* If condition is successfull, then register has value 1. *)
                            ], 
                            T.TEMP reg (* The register has the output value of the condition. *)
                        )
            end
    
    fun unNx (Ex e) = T.EXP e
        | unNx (Nx s) = s
        | unNx (c as Cx func) = let val e = Ex (unEx c) in unNx e  end

    (* 
    All the three cases in unEx (Ex e) can be achieved using the 3rd case but
    unconditional jumps give efficient translation, since the jump might be removed
    while reordering the statements in traceSchedule function.
    *)
    fun unCx (Ex e) =
            (case e of
                T.CONST 1 => (fn (succ, fail) => T.JUMP (T.NAME succ, [succ]))
                (* If e is 0, jump to failure label *)
                | T.CONST 0 => (fn (succ, fail) => T.JUMP (T.NAME fail, [fail]))
                (* If none, compare e and 1 and jump accordingly *)
                | _ => (fn (succ, fail) => T.CJUMP (T.EQ, e, T.CONST 1, succ, fail)))
        | unCx (Cx func) = func
        | unCx (e as Nx s) = unCx (Ex (unEx e))
    
    fun map f l = foldr (fn (x,a) => (f x)::a) [] l

    fun translateIfElse (e1, e2, e3) = 
        let
            val t = Temp.newlabel()
            val f = Temp.newlabel()
        in
            (case e3 of
                NONE => T.ESEQ (
                        seq ([
                            (unCx e1) (t, f),
                            T.LABEL t,
                            unNx e2,
                            T.LABEL f
                        ]), T.CONST 0
                    )
                | SOME e => 
                    let
                        val r = Temp.newtemp()
                        val join = Temp.newlabel()
                    in
                        T.ESEQ (
                                seq ([
                                    (unCx e1) (t, f),
                                    T.LABEL t,
                                    T.MOVE (T.TEMP r, unEx e2),
                                    T.JUMP (T.NAME join, [join]),
                                    T.LABEL f,
                                    T.MOVE (T.TEMP r, unEx e),
                                    T.LABEL join
                                ]), T.TEMP r
                            )
                    end)
        end
    
    fun translate (A.Expr exp) = T.EXP (translateExp (table, exp))
        | translate (A.Decs decs) = translateDecs (table, decs)

    and translateDec (table, dec) =
        (case dec of
            (A.VarDec {name, type_, init}) =>
                (let
                    val t = Temp.newtemp()
                    val exp_trans = translateExp (table, init)
                in
                    (table := Symbol.enter (!table, name, T.TEMP t);
                    T.MOVE (T.TEMP t, exp_trans))
                end)
        |   _ => raise Unimplemented "Declaration")

    and translateDecs_ (table, []) = [nop]
        | translateDecs_ (table, (x::xs)) = (translateDec (table, x)) 
                                        :: (translateDecs_ (table, xs))

    and translateDecs (table, []) = nop
        | translateDecs (table, ls) = seq (translateDecs_ (table, ls))

    and translateExps_ (table, []) = T.CONST 0
        | translateExps_ (table, (x::xs)) =
            T.ESEQ (T.EXP (translateExp (table, x)), (translateExps_ (table, xs)))

    and translateExps (table, []) = T.CONST 0
        | translateExps (table, ls) = translateExps_ (table, ls)

    and translateLvalue (table, lval) =
        (case lval of
            A.SimpleVar v => 
                let
                    val look = Symbol.look (!table, v)
                in
                    (case look of
                        SOME t => t
                    |   NONE => raise UndefinedVariable (Symbol.name v))
                end    
        |   A.FieldVar {object, name} => raise Unimplemented "Field Variable"
        |   A.SubscriptVar {object, index} => raise Unimplemented "Subscript Variable")

    and translateConditionExp (table, exp) =
        case exp of
            A.IntExp n =>
                (case n of
                    0 => unCx (Ex (T.CONST 0))
                    | _ => unCx (Ex (T.CONST 1)))
            | A.OpExp {left, oper, right} =>
                let
                    val oper' = (case oper of
                        A.Eq => RelOp T.EQ
                    |   A.Ne => RelOp T.NE
                    |   A.Gt => RelOp T.GE
                    |   A.Lt => RelOp T.LT
                    |   A.Ge => RelOp T.GE
                    |   A.Le => RelOp T.LE
                    |   _ => raise SyntaxError)
                in
                    case oper' of
                        RelOp oper => 
                            (fn (succ, fail) => T.CJUMP (oper, translateExp (table, left), translateExp (table, right), succ, fail))
                        | _ => raise TranslationError
                end
            | _ => raise SyntaxError

    and translateExp (table, exp) = 
        (case exp of
            A.NilExp => T.CONST 0
        |   A.IntExp n => T.CONST n
        |   A.LetExp {decs, body} => 
                let
                    val decs_trans = translateDecs (table, decs)
                    val body_trans = translateExps (table, body)
                in
                    T.ESEQ (decs_trans, body_trans)
                end
        |   A.LvalExp lval => 
                translateLvalue (table, lval) 
        |   A.Negate exp =>
                let
                    val exp_trans = translateExp (table, exp)
                in
                    T.BINOP (T.MINUS, T.CONST 0, exp_trans)
                end
        |   A.OpExp {left, oper, right} =>
                let
                    val left_trans = translateExp (table, left)
                    val right_trans = translateExp (table, right)
                    val oper' = (case oper of
                        A.Plus => BinOp T.PLUS
                    |   A.Minus => BinOp T.MINUS
                    |   A.Mul => BinOp T.MUL
                    |   A.Div => BinOp T.DIV
                    |   A.Eq => RelOp T.EQ
                    |   A.Ne => RelOp T.NE
                    |   A.Gt => RelOp T.GE
                    |   A.Lt => RelOp T.LT
                    |   A.Ge => RelOp T.GE
                    |   A.Le => RelOp T.LE
                    |   A.And => BinOp T.AND
                    |   A.Or => BinOp T.OR)
                in
                    case oper' of
                       BinOp oper => 
                            T.BINOP (oper, left_trans, right_trans)
                    |  RelOp oper => 
                            let
                                val t = Temp.newlabel ()
                                val f = Temp.newlabel ()
                                val stm = T.CJUMP (oper, left_trans, right_trans, t, f)
                            in
                                unEx (Nx stm)
                            end
                            
                end
        |   A.SeqExp exps => translateExps (table, exps)
        |   A.AssignExp {object, exp} =>
                let
                    val lval = translateLvalue (table, object)
                    val exp = translateExp (table, exp)
                in
                    unEx (Nx (T.MOVE (lval, exp)))
                end
        |   A.IfElseExp {cond, succ, fail} =>
                let
                    val t = Temp.newlabel ()
                    val f = Temp.newlabel ()
                    val c = Temp.newlabel ()
                    val cond_trans = translateConditionExp (table, cond)
                    (* val cond_trans = translateExp (table, cond)
                    val cond_trans = unCx (Ex cond_trans) *)
                    val succ_trans = unNx (Ex (translateExp (table, succ)))
                    val fail_trans = (case fail of
                                        NONE => NONE
                                        | SOME e => 
                                            let
                                                val fail_trans = translateExp (table, e)
                                            in
                                                SOME (Ex fail_trans)
                                            end)
                in
                    case fail_trans of
                        NONE =>
                            T.ESEQ (
                                seq([
                                    cond_trans (t, c),
                                    T.LABEL t,
                                    succ_trans,
                                    T.LABEL c
                                ]), T.CONST 0
                            )
                     | SOME trans =>
                            T.ESEQ (
                                seq([
                                    cond_trans (t, f),
                                    T.LABEL t,
                                    succ_trans,
                                    T.JUMP (T.NAME c, [c]),
                                    T.LABEL f,
                                    unNx trans,
                                    T.LABEL c
                                ]), T.CONST 0
                            )
                end
        |   A.WhileExp {cond, body} =>
                let
                    val test = Temp.newlabel ()
                    val done = Temp.newlabel ()
                    val continue = Temp.newlabel ()
                    val cond_trans = translateConditionExp (table, cond)
                    val body_trans = translateExp (table, body)
                in
                    T.ESEQ (
                        seq ([
                            T.LABEL test,
                            cond_trans (continue, done),
                            T.LABEL continue,
                            unNx (Ex body_trans),
                            T.JUMP (T.NAME test, [test]),
                            T.LABEL done
                        ]), T.CONST 0
                    )
                end
        |   A.ForExp {init = {name, exp}, exit_cond, body} =>
                let
                    val t = Temp.newtemp()
                    val done = Temp.newlabel ()
                    val continue = Temp.newlabel ()
                    val test = Temp.newlabel ()
                    val init_exp = translateExp (table, exp)
                    val cond_trans = translateConditionExp (table, exit_cond)
                    val body_trans = translateExp (table, body)
                in
                    (table := Symbol.enter (!table, name, T.TEMP t));
                    T.ESEQ (
                        seq ([
                            T.MOVE (T.TEMP t, init_exp),
                            T.LABEL test,
                            cond_trans (continue, done),
                            T.LABEL continue,
                            unNx (Ex body_trans),
                            T.JUMP (T.NAME test, [test]),
                            T.LABEL done
                        ]), T.CONST 0
                    )
                end
        |   _ => raise Unimplemented "Expression" )
        handle UndefinedVariable x => (print ("Undefined variable: " ^ x ^ "\n"); OS.Process.exit OS.Process.failure)
        handle Unimplemented x => (print ("Unimplemented Translation: " ^ x ^ "\n"); OS.Process.exit OS.Process.failure)

end