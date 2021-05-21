signature SEMANT =
sig
    type frame
    type venv
    val transExp: venv * frame -> Tiger.Exp -> Tree.exp
    val transDec: venv * frame -> Tiger.Dec -> (venv * Tree.stm)
    val transTiger: venv * frame * Tiger.ast -> Tree.exp
    val transProg: Tiger.ast -> Tree.stm
end

structure Semant: SEMANT =
struct

    type frame = Frame.frame
    type venv = Env.enventry Symbol.table

    structure A = Tiger
    structure T = Tree
    structure Tr = Translate
    structure S = Symbol
    structure F = Frame
    structure E = Env
    exception Error
    exception Unimplemented
    exception SyntaxError
    exception TranslationError
    exception Undefined of string

    fun map f l = foldr (fn (x,a) => (f x)::a) [] l

    fun merge (a::as_) (b::bs_) = (a, b)::(merge as_ bs_)
                        | merge [] [] = []
                        | merge _ _ = raise SyntaxError

    datatype Op = RelOp of Tree.relop
                | BinOp of Tree.binop

    fun transTiger (venv, curFrame, A.Expr exp) = transExp (venv, curFrame) exp
        | transTiger (venv, curFrame, A.Decs decs) = raise Error

    and transDec (venv, curFrame) =
        let
            fun trdec dec =
                (case dec of
                    A.VarDec {name, type_, init} =>
                        let
                            val local_ = F.allocLocal curFrame false
                            val dest = (F.getAccessExp local_) (T.TEMP F.FP)
                            val venv' = Symbol.enter (
                                venv,
                                name,
                                E.VarEntry {access=local_, type_=type_}
                            )
                            val init = transExp (venv', curFrame) init
                        in
                            (venv', T.MOVE (dest, init))
                        end
                |   A.FunDec {name, args, ret, body} =>
                        let
                            val funcLabel = Temp.newlabel ()
                            val skipFuncLabel = Temp.newlabel ()
                            val escape_args = map (fn _ => true) args
                            val newFrame = F.newFrame {name=name, args=escape_args}
                            fun addArgsToEnv (venv, []) = venv
                                | addArgsToEnv (venv, ({name, type_}, arg)::xs) =
                                    let
                                        val enventry = E.VarEntry {
                                            access=arg,
                                            type_=SOME type_
                                        }
                                        val venv' = Symbol.enter (
                                            venv,
                                            name,
                                            enventry
                                        )
                                        val venv' = addArgsToEnv (venv', xs)
                                    in
                                        venv'
                                    end
                            val venv' = addArgsToEnv (venv, merge args (F.args newFrame))
                            val RA = T.BINOP (T.MINUS, T.TEMP F.FP, T.CONST F.ws) 
                            val venv' = Symbol.enter (
                                venv',
                                name,
                                E.FunEntry {
                                    frame=newFrame,
                                    label=funcLabel, 
                                    args=args, 
                                    ret=ret
                                }
                            )
                            val body = transExp (venv', curFrame) body
                        in
                            (venv', Tr.seq ([
                                    T.JUMP (T.NAME skipFuncLabel, [skipFuncLabel]),
                                    T.LABEL funcLabel,
                                    T.MOVE (T.TEMP F.RV, body),
                                    T.JUMP (RA, []),
                                    T.LABEL skipFuncLabel
                                ])
                            )
                        end
                |   A.TypeDec _ => raise Unimplemented
                |   A.ClassDec _ => raise Unimplemented
                |   A.PrimitiveDec _ => raise Unimplemented)
        in
            trdec
        end

    and transExp (venv, curFrame) = 
        let
            fun trvar var =
            (case var of
                A.SimpleVar id =>
                    (case S.look (venv, id) of
                        NONE => raise Undefined (Symbol.name id)
                    |   SOME enventry =>
                            (case enventry of
                                E.VarEntry {access, type_} => (F.getAccessExp access) (T.TEMP F.FP)
                            |   _ => raise Undefined (Symbol.name id))
                    )
            |   _ => raise Unimplemented)
            and trcep exp =
                (case exp of
                    A.IntExp n =>
                        (case n of
                            0 => Tr.unCx (Tr.Ex (T.CONST 0))
                            | _ => Tr.unCx (Tr.Ex (T.CONST 1)))
                    | A.OpExp {left, oper, right} =>
                        let
                            val oper' = (case oper of
                                A.Eq => RelOp T.EQ
                            |   A.Ne => RelOp T.NE
                            |   A.Gt => RelOp T.GT
                            |   A.Lt => RelOp T.LT
                            |   A.Ge => RelOp T.GE
                            |   A.Le => RelOp T.LE
                            |   _ => raise SyntaxError)
                        in
                            case oper' of
                                RelOp oper => 
                                    (fn (succ, fail) => T.CJUMP (oper, trexp left, trexp right, succ, fail))
                                | _ => raise TranslationError
                        end
                    | A.SeqExp exps =>
                        let
                            val len = List.length exps
                            val last = List.nth (exps, len - 1)
                        in
                            trcep last
                        end
                    | _ => raise SyntaxError)
            and trexps_ [] = T.CONST 0
                | trexps_ (x::xs) =
                    T.ESEQ (T.EXP (trexp x), trexps_ xs)
            and trexps [] = T.CONST 0
                | trexps ls = trexps_ ls
            and trexp exp = 
                (case exp of
                    A.NilExp => T.CONST 0
                |   A.IntExp i => T.CONST i
                |   A.StringExp _ => raise Unimplemented
                |   A.ArrayExp _ => raise Unimplemented
                |   A.RecordExp _ => raise Unimplemented
                |   A.New _ => raise Unimplemented
                |   A.LvalExp e => trvar e
                |   A.FunctionCall {name, args} =>
                        let
                            fun getArgs [] = []
                                | getArgs (x::xs) =
                                    ((F.getAccessExp x) (T.TEMP F.FP))::(getArgs xs)

                            val funcCall = (case Symbol.look (venv, name) of
                                                SOME (E.FunEntry {frame, label, args, ret}) => 
                                                    T.EXP (T.CALL (T.NAME label, getArgs (F.args frame)))
                                                |   _  => raise Undefined "function"
                                            )
                            fun getFunctionFrame () = (case Symbol.look (venv, name) of
                                                        SOME (E.FunEntry {frame, label, args, ret}) => 
                                                            frame
                                                        |   _  => raise Undefined "function"
                                                    )

                            fun getPushLocation curOffset =
                                T.BINOP (T.MINUS, T.TEMP F.SP, T.CONST (curOffset * F.ws))
                            val returnLabel = Temp.newlabel ()
                            val pushFP = T.MOVE (T.MEM (getPushLocation 1), T.TEMP F.FP)
                            val pushRetAddr = T.MOVE (T.MEM (getPushLocation 2), T.NAME returnLabel)
                            fun pushArgs [] i = []
                                | pushArgs (x::xs) i =
                                    T.MOVE (T.MEM (getPushLocation i), trexp x)::(pushArgs xs (i + 1))
                            
                            val curOffset = (List.length args) + 2
                            val moveFP = T.MOVE (T.TEMP F.FP, T.BINOP (T.MINUS, T.TEMP F.SP, T.CONST F.ws))
                            val moveSP = T.MOVE (T.TEMP F.SP, T.BINOP (T.MINUS, T.TEMP F.SP, T.CONST (curOffset * F.ws)))
                            val resetFP = T.MOVE (T.TEMP F.FP, T.MEM (T.TEMP F.FP))
                            val resetSP = T.MOVE (T.TEMP F.SP, T.BINOP (T.PLUS, T.TEMP F.FP, T.CONST F.ws))
                        in
                            T.ESEQ (
                                Tr.seq ([
                                        pushFP,
                                        pushRetAddr
                                    ] @ pushArgs args 3 @ [
                                        moveFP,
                                        moveSP,
                                        funcCall,
                                        T.LABEL returnLabel,
                                        resetSP,
                                        resetFP
                                    ]), T.TEMP F.RV
                            )
                        end
                |   A.MethodCall {object, name, args} => raise Unimplemented
                |   A.Negate exp =>
                        let
                            val exp = trexp exp
                        in
                            T.BINOP (T.MINUS, T.CONST 0, exp)
                        end
                |   A.OpExp {left, oper, right} =>
                        let
                            val left = trexp left
                            val right = trexp right
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
                                    T.BINOP (oper, left, right)
                            |   RelOp oper =>
                                    let
                                        val t = Temp.newlabel ()
                                        val f = Temp.newlabel ()
                                        val stm = T.CJUMP (oper, left, right, t, f)
                                    in
                                        Tr.unEx (Tr.Nx stm)
                                    end    
                        end
                |   A.SeqExp exps => trexps exps
                |   A.AssignExp {object, exp} =>
                        let
                            val object = trvar object
                            val exp = trexp exp
                        in
                            Tr.unEx (Tr.Nx (T.MOVE (object, exp)))
                        end
                |   A.IfElseExp {cond, succ, fail} =>
                        let
                            val t = Temp.newlabel ()
                            val f = Temp.newlabel ()
                            val finish = Temp.newlabel ()
                            val result = Temp.newtemp ()
                            val cond = trcep cond
                            val succ = trexp succ
                            val fail = 
                                (case fail of
                                    NONE => NONE
                                |   SOME e => SOME (trexp e))
                        in
                            case fail of
                                NONE =>
                                    T.ESEQ (
                                        Tr.seq ([
                                            cond (t, finish),
                                            T.LABEL t,
                                            Tr.unNx (Tr.Ex succ),
                                            T.LABEL finish
                                        ]), T.CONST 0
                                    )
                            |   SOME fail =>
                                    T.ESEQ (
                                        Tr.seq([
                                            cond (t, f),
                                            T.LABEL t,
                                            T.MOVE (T.TEMP result, succ),
                                            T.JUMP (T.NAME finish, [finish]),
                                            T.LABEL f,
                                            T.MOVE (T.TEMP result, fail),
                                            T.LABEL finish
                                        ]), T.TEMP result
                                    )
                        end
                |   A.WhileExp {cond, body} => 
                        let
                            val test = Temp.newlabel ()
                            val finish = Temp.newlabel ()
                            val continue = Temp.newlabel ()
                            val cond = trcep cond
                            val body = trexp body
                        in
                            T.ESEQ (
                                Tr.seq ([
                                    T.LABEL test,
                                    cond (continue, finish),
                                    T.LABEL continue,
                                    Tr.unNx (Tr.Ex body),
                                    T.JUMP (T.NAME test, [test]),
                                    T.LABEL finish
                                ]), T.CONST 0
                            )
                        end
                |   A.ForExp {init = {name, exp}, exit_cond, body} => 
                        let
                            val t = Temp.newtemp()
                            val finish = Temp.newlabel ()
                            val continue = Temp.newlabel ()
                            val test = Temp.newlabel ()
                            val init = trexp exp
                            val cond = trcep exit_cond
                            val vardec = A.VarDec {name=name, type_=NONE, init=exp}
                            val (venv', dec) = transDec (venv, curFrame) vardec
                        in
                            let
                                val body = transExp (venv', curFrame) body
                            in
                                T.ESEQ (
                                    Tr.seq ([
                                        T.MOVE (T.TEMP t, init),
                                        T.LABEL test,
                                        cond (continue, finish),
                                        T.LABEL continue,
                                        T.MOVE (T.TEMP t, T.BINOP (T.PLUS, T.TEMP t, T.CONST 1)),
                                        Tr.unNx (Tr.Ex body),
                                        T.JUMP (T.NAME test, [test]),
                                        T.LABEL finish
                                    ]), T.CONST 0
                                )
                            end
                        end
                |   A.BreakExp => raise Unimplemented
                |   A.LetExp {decs, body} =>
                        let
                            fun trdecs (venv, []) = (venv, [])
                                | trdecs (venv, (x::xs)) =
                                    let
                                        fun trdec (venv, dec) = transDec (venv, curFrame) dec
                                        val (venv', dec) = trdec (venv, x)
                                        val (venv', decs) = trdecs (venv', xs)
                                    in
                                        (venv', dec::decs)
                                    end
                            val (venv', decs) =
                                let
                                    val (venv', decs) = trdecs (venv, decs)
                                in
                                    (venv', Tr.seq decs)
                                end
                            val body = transExp (venv', curFrame) (A.SeqExp body)
                        in
                            T.ESEQ (decs, body)
                        end
                )
        in
            trexp
        end
    
    fun transProg ast =
        let
            val venv = E.base_venv
            val curFrame = F.newFrame {
                name=Temp.namedlabel "main",
                args = []
            }
            val exp = transTiger (venv, curFrame, ast)
        in
            T.EXP exp
        end
end