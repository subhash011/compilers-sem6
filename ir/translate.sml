(* Translating tree to IR *)

signature TRANSLATE =
sig
    type frame
    type venv
    datatype exp =  Ex of Tree.exp
                    | Nx of Tree.stm
                    | Cx of Temp.label * Temp.label -> Tree.stm
    val unEx: exp -> Tree.exp
    val unNx: exp -> Tree.stm
    val unCx: exp -> Temp.label * Temp.label -> Tree.stm
    val transExp: venv * frame -> Tiger.Exp -> Tree.exp
    val transDec: venv * frame -> Tiger.Dec -> (venv * Tree.stm)
    val transTiger: venv * frame * Tiger.ast -> Tree.exp
    val transProg: Tiger.ast -> Tree.stm
end

structure Translate: TRANSLATE =
struct

    type frame = Frame.frame
    type venv = Env.enventry Symbol.table

    datatype exp =  Ex of Tree.exp
                    | Nx of Tree.stm
                    | Cx of Temp.label * Temp.label -> Tree.stm

    structure A = Tiger
    structure T = Tree
    structure S = Symbol
    structure F = Frame
    structure E = Env
    exception TranslationError
    exception SyntaxError of string
    exception Unimplemented of string
    exception Undefined of string

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

    (* Stores the current context of the break statement. *)
    val curBreak: Temp.label ref = ref (Temp.namedlabel "NONE")

    fun printErrorString s =    (TextIO.output(TextIO.stdErr, s); OS.Process.exit OS.Process.failure)

    fun map f l = foldr (fn (x,a) => (f x)::a) [] l

    fun merge (a::as_) (b::bs_) = (a, b)::(merge as_ bs_)
                        | merge [] [] = []
                        | merge _ _ = raise SyntaxError "invalid number of args supplied"

    datatype Op = RelOp of Tree.relop
                | BinOp of Tree.binop

    (* Translate the ast. *)
    fun transTiger (venv, curFrame, A.Expr exp) = transExp (venv, curFrame) exp
        | transTiger (venv, curFrame, A.Decs decs) =  
            (* Translating list of declaration *)
            let
                fun trdecs venv [] = []
                    | trdecs venv [x] = 
                        let 
                            val (_, stm) = transDec (venv, curFrame) x
                        in
                            [stm]
                        end
                    | trdecs venv (x::xs) = 
                        let 
                            val (venv', stm) = transDec (venv, curFrame) x
                        in
                            stm::(trdecs venv' xs)
                        end
            in
                T.ESEQ (
                    seq (trdecs venv decs),
                    T.CONST 0
                )
            end

    and transDec (venv, curFrame) =
        let
            fun trdec dec =
                (case dec of
                    A.VarDec {name, type_, init} =>
                        let
                            val local_ = F.allocLocal curFrame false
                            val dest = (F.getAccessExp local_) (T.TEMP F.FP)
                            (* Add the declared variable and modify the environment. *)
                            val venv' = Symbol.enter (
                                venv,
                                name,
                                E.VarEntry {access=local_, type_=type_}
                            )
                            (* translate the initialiser *)
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
                            val RA = T.BINOP (T.MINUS, T.TEMP F.FP, T.CONST F.ws) 
                            val venv' = Symbol.enter (
                                venv,
                                name,
                                E.FunEntry {
                                    frame=newFrame,
                                    label=funcLabel, 
                                    args=args, 
                                    ret=ret
                                }
                            )
                            val body = transExp (
                                addArgsToEnv (venv', 
                                    merge args (F.args newFrame)
                                ), 
                                curFrame
                                ) body
                        in
                            (venv', seq ([
                                    T.JUMP (T.NAME skipFuncLabel, [skipFuncLabel]), (* Slip the function if not from CALL *)
                                    T.LABEL funcLabel,
                                    T.MOVE (T.TEMP F.RV, body),
                                    T.JUMP (RA, []), (* Jump to Return address on stack. *)
                                    T.LABEL skipFuncLabel
                                ])
                            )
                        end
                |   A.TypeDec _ => raise Unimplemented "Type declaration"
                |   A.ClassDec _ => raise Unimplemented "Class declaration"
                |   A.PrimitiveDec _ => raise Unimplemented "Primitive declaration")
        in
            trdec
        end

    and transExp (venv, curFrame) = 
        let
            fun trvar var =
            (case var of
                A.SimpleVar id =>
                    (* If variable exists in the environment, translate it *)
                    (case S.look (venv, id) of
                        NONE => raise Undefined ("variable declaration for \"" ^ (Symbol.name id) ^ "\" not found.")
                    |   SOME enventry =>
                            (case enventry of
                                E.VarEntry {access, type_} => (F.getAccessExp access) (T.TEMP F.FP)
                            |   _ => raise Undefined ("variable declaration for \"" ^ (Symbol.name id) ^ "\" not found,\
                                                        \ did you mean to call a function ?"))
                    )
            |   _ => raise Unimplemented "Arrays and records")
            (*  The below function is used to translate if else statements
                without a list of statements (SeqExp). If it has a 'then', it
                returns else it performs side-effect tasks. *)
            and trcep {cond, succ, fail} =
                let
                    val t = Temp.newlabel ()
                    val f = Temp.newlabel ()
                    val finish = Temp.newlabel ()
                    val result = Temp.newtemp ()
                    val cond = unCx (Ex (trexp cond))
                    val succ = trexp succ
                    val fail = 
                        (case fail of
                            NONE => NONE
                        |   SOME e => SOME (trexp e))
                in
                    case fail of
                        NONE =>
                            T.ESEQ (
                                seq ([
                                    cond (t, finish),
                                    T.LABEL t,
                                    unNx (Ex succ),
                                    T.LABEL finish
                                ]), T.CONST 0
                            )
                    |   SOME fail =>
                            T.ESEQ (
                                seq([
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
            (* Translate list of expressions and return the value of the last expression *)
            and trexps [] = T.CONST 0
                | trexps [x] = trexp x
                | trexps (x::xs) = T.ESEQ (T.EXP (trexp x), trexps xs)
            and trexp exp = 
                (case exp of
                    A.NilExp => T.CONST 0
                |   A.IntExp i => T.CONST i
                |   A.StringExp _ => raise Unimplemented "Strings"
                |   A.ArrayExp _ => raise Unimplemented "Arrays"
                |   A.RecordExp _ => raise Unimplemented "Records"
                |   A.New _ => raise Unimplemented "Objects"
                |   A.LvalExp e => trvar e 
                |   A.FunctionCall {name, args as funargs} =>
                        let
                            fun getArgs [] = []
                                | getArgs (x::xs) =
                                    ((F.getAccessExp x) (T.TEMP F.FP))::(getArgs xs)

                            val funcCall = (case Symbol.look (venv, name) of
                                                SOME (E.FunEntry {frame, label, args, ret}) => 
                                                    if (List.length funargs) = (List.length args)
                                                    then T.EXP (T.CALL (T.NAME label, getArgs (F.args frame)))
                                                    else raise SyntaxError ("Invalid number of arguments for function \"" ^ (Symbol.name name) ^ "\"")
                                                |   _  => raise Undefined ("function declaration for \"" ^ (Symbol.name name) ^ "\" not found.")
                                            )
                            fun getFunctionFrame () = (case Symbol.look (venv, name) of
                                                        SOME (E.FunEntry {frame, label, args, ret}) => 
                                                            frame
                                                        |   _  => raise Undefined ("function declaration for \"" ^ (Symbol.name name) ^ "\" not found.")
                                                    )

                            fun getPushLocation curOffset =
                                T.BINOP (T.MINUS, T.TEMP F.SP, T.CONST (curOffset * F.ws))
                            val returnLabel = Temp.newlabel ()
                            val pushFP = T.MOVE (T.MEM (getPushLocation 1), T.TEMP F.FP)
                            val pushRetAddr = T.MOVE (T.MEM (getPushLocation 2), T.NAME returnLabel)
                            fun pushArgs [] i = []
                                | pushArgs (x::xs) i =
                                    T.MOVE (T.MEM (getPushLocation i), trexp x)::(pushArgs xs (i + 1))
                            fun pushLocals [] _ = []
                                | pushLocals ((k, v)::xs) i =
                                    let
                                        val dest = getPushLocation i
                                        val inst = (case v of
                                            E.VarEntry {access, type_} =>
                                                T.MOVE (dest, (F.getAccessExp access) (T.CONST 0))
                                        |   E.FunEntry _ => T.EXP (T.CONST 0))
                                    in
                                        inst::(pushLocals xs (i + 1))
                                    end

                            fun restoreLocals [] _ = []
                                | restoreLocals ((k, v)::xs) i =
                                    let
                                        val dest = getPushLocation i
                                        val inst = (case v of
                                            E.VarEntry {access as (F.InReg reg), type_} =>
                                                T.MOVE (T.TEMP reg, dest)
                                        |   _ => T.EXP (T.CONST 0))
                                    in
                                        inst::(restoreLocals xs (i + 1))
                                    end
                            fun getNumLocals [] = 0
                                | getNumLocals ((k, v)::xs) =
                                    let
                                        val add = (case v of
                                           E.VarEntry _ => 1
                                         | E.FunEntry _ => 0)
                                    in
                                        add + (getNumLocals xs)
                                    end
                            val locals = Symbol.listItemsi venv
                            val numArgs = List.length args
                            val numLocals = getNumLocals locals
                            val curOffset = 2 + numArgs + numLocals
                            val moveFP = T.MOVE (T.TEMP F.FP, T.BINOP (T.MINUS, T.TEMP F.SP, T.CONST F.ws))
                            val moveSP = T.MOVE (T.TEMP F.SP, T.BINOP (T.MINUS, T.TEMP F.SP, T.CONST (curOffset * F.ws)))
                            val resetFP = T.MOVE (T.TEMP F.FP, T.MEM (T.TEMP F.FP))
                            val resetSP = T.MOVE (T.TEMP F.SP, T.BINOP (T.PLUS, T.TEMP F.FP, T.CONST F.ws))
                        in
                            T.ESEQ (
                                seq ([
                                        pushFP, (* push frame pointer onto the stack *)
                                        pushRetAddr (* push return address *)
                                    ] @ pushLocals locals 3 (* move local arguments from register to stack. *)
                                      @ pushArgs args (numLocals + 3) @ [ (* push function parameters. *)
                                        moveFP, (* move frame pointer and stack pointer. *)
                                        moveSP,
                                        funcCall, (* Call the function. *)
                                        T.LABEL returnLabel, (* function returns here. *)
                                        resetSP, (* delete the frame *)
                                        resetFP
                                    ] @ restoreLocals locals 3), 
                                    (*  this call returns the value in the special RV register which stores the
                                        function output.*)
                                    T.TEMP F.RV
                            )
                        end
                |   A.MethodCall {object, name, args} => raise Unimplemented "Classes and methods"
                |   A.Negate exp =>
                        let
                            val exp = trexp exp
                        in
                            T.BINOP (T.MINUS, T.CONST 0, exp)
                        end
                |   A.OpExp {left, oper, right} =>
                        let
                            val t = Temp.newlabel ()
                            val f = Temp.newlabel ()
                            val s = Temp.newlabel ()
                            val r = Temp.newtemp ()
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
                                    (*  OR and AND must be handled specially. They return 1 for true
                                        and 0 for false. *)
                                    (case oper of
                                            T.AND => 
                                            let
                                                val stm = seq ([
                                                            T.MOVE (T.TEMP r, T.CONST 1),
                                                            T.CJUMP (T.EQ, left, T.CONST 1, s, f),
                                                            T.LABEL s,
                                                            T.CJUMP (T.EQ, right, T.CONST 1, t, f),
                                                            T.LABEL f,
                                                            T.MOVE (T.TEMP r, T.CONST 0),
                                                            T.LABEL t
                                                        ])
                                            in
                                                T.ESEQ (
                                                    stm, 
                                                    T.TEMP r
                                                )
                                            end
                                        |   T.OR => 
                                                let
                                                    val stm = seq ([
                                                                T.MOVE (T.TEMP r, T.CONST 1),
                                                                T.CJUMP (T.EQ, left, T.CONST 1, t, s),
                                                                T.LABEL s,
                                                                T.CJUMP (T.EQ, right, T.CONST 1, t, f),
                                                                T.LABEL f,
                                                                T.MOVE (T.TEMP r, T.CONST 0),
                                                                T.LABEL t
                                                            ])
                                                in
                                                    T.ESEQ (
                                                        stm, 
                                                        T.TEMP r
                                                    )
                                                end
                                        |   _   => T.BINOP (oper, left, right))
                            |   RelOp oper =>
                                    let
                                        val exp = Cx (fn (a, b) => T.CJUMP (oper, left, right, a, b))
                                    in
                                        unEx exp
                                    end 
                        end
                |   A.SeqExp exps => trexps exps
                |   A.AssignExp {object, exp} =>
                        let
                            val object = trvar object
                            val exp = trexp exp
                        in
                            unEx (Nx (T.MOVE (object, exp)))
                        end
                |   A.IfElseExp (e as {cond, succ, fail}) =>
                        (case cond of
                            A.SeqExp exps =>
                                let
                                    val expsCount = List.length exps
                                in
                                    if expsCount = 0
                                    then raise SyntaxError "If needs a condition"
                                    else trcep e
                                end
                        |   _ => trcep e
                            )
                |   A.WhileExp {cond, body} => 
                        let
                            val test = Temp.newlabel ()
                            val finish = Temp.newlabel ()
                            val continue = Temp.newlabel ()
                            val cond = unCx (Ex (trexp cond))
                            (*  the curBreak stores the finish label of this
                                to jump to the end when called. *)
                            val body = (curBreak := finish; trexp body)
                        in
                            T.ESEQ (
                                seq ([
                                    T.LABEL test,
                                    cond (continue, finish),
                                    T.LABEL continue,
                                    unNx (Ex body),
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
                            val exit_cond = trexp exit_cond
                            val vardec = A.VarDec {name=name, type_=NONE, init=exp}
                            val (venv', dec) = transDec (venv, curFrame) vardec
                            (*  the curBreak stores the finish label of this
                                to jump to the end when called. *)
                            val body = (curBreak := finish; transExp (venv', curFrame) body)
                        in
                            T.ESEQ (
                                seq ([
                                    T.MOVE (T.TEMP t, init),
                                    T.LABEL test,
                                    T.CJUMP (T.LE, T.TEMP t, exit_cond, continue, finish),
                                    T.LABEL continue,
                                    T.MOVE (T.TEMP t, T.BINOP (T.PLUS, T.TEMP t, T.CONST 1)),
                                    unNx (Ex body),
                                    T.JUMP (T.NAME test, [test]),
                                    T.LABEL finish
                                ]), T.CONST 0
                            )
                        end
                |   A.BreakExp =>
                        if (Symbol.name (!curBreak)) = "NONE"
                        then raise SyntaxError "Invalid break detected, is it inside a loop ?"
                        else T.ESEQ (
                            seq [
                                T.JUMP (T.NAME (!curBreak), [!curBreak]),
                                T.LABEL (Temp.newlabel ())
                            ]
                            , T.CONST 0)
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
                            (* Modify the environment with variable declarations. *)
                            val (venv', decs) =
                                let
                                    val (venv', decs) = trdecs (venv, decs)
                                in
                                    (venv', seq decs)
                                end
                            (* Build the body using the modified expression. *)
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
            (* the initial frame named main. *)
            val curFrame = F.newFrame {
                name=Temp.namedlabel "main",
                args = []
            }
            val exp = transTiger (venv, curFrame, ast)
                        handle Unimplemented s => (printErrorString ("Unimplemented expression: " ^ s ^ "\n"))
                            | SyntaxError s => (printErrorString ("Syntax error: " ^ s ^ "\n"))
                            | Undefined s => (printErrorString ("Undefined: " ^ s ^ "\n"))
        in
            T.EXP exp
        end
end