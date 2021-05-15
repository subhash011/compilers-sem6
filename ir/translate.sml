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
    exception Unimplemented
    exception UndefinedVariable of string

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
        |   _ => raise Unimplemented)

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
        |   A.FieldVar {object, name} => raise Unimplemented
        |   A.SubscriptVar {object, index} => raise Unimplemented)

    and translateExp (table, exp) = 
        (case exp of
            A.NilExp => T.CONST 0
        |   A.IntExp n => T.CONST n
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
        |   A.LetExp {decs, body} => 
                let
                    val decs_trans = translateDecs (table, decs)
                    val body_trans = translateExps (table, body)
                in
                    T.ESEQ (decs_trans, body_trans)
                end
        | A.LvalExp lval => 
            translateLvalue (table, lval) 
        | _ => raise Unimplemented)
        handle UndefinedVariable x => (print ("Undefined variable: " ^ x ^ "\n"); OS.Process.exit OS.Process.failure)

end