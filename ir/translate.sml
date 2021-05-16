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
    (* Some special registers for return address and return value *)
    val ReturnAddressRegister = T.TEMP (Temp.newtemp ())
    val ReturnValueRegister = T.TEMP (Temp.newtemp ())
    val nop = T.EXP (T.CONST 0)

    (* holds variable mappings *)
    val table: Tree.exp Symbol.table ref = ref Symbol.empty
    (* holds function mappings *)
    val funcDecTable: Tree.stm Symbol.table ref = ref Symbol.empty
    val funcTable: (Tree.exp list) Symbol.table ref = ref Symbol.empty

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
                            unNx e2, (* If there is no else, we perform side effects. *)
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
    
    fun translate (A.Expr exp) = T.EXP (translateExp (exp))
        | translate (A.Decs decs) = translateDecs (decs)

    and translateDec (dec) =
        (case dec of
            A.VarDec {name, type_, init} =>
                (let
                    val t = Temp.newtemp()
                    val exp_trans = translateExp (init)
                in
                    (table := Symbol.enter (!table, name, T.TEMP t);
                    T.MOVE (T.TEMP t, exp_trans))
                end)
            | A.FunDec {name, args, ret, body} =>
                let
                    val funcLabel = Temp.newlabel ()
                    val jumpfunct = Temp.newlabel ()
                    val retreg = Temp.newtemp ()
                    val args_regs = 
                        let
                            fun arguments nil = []
                                | arguments (x::xs) = 
                                    let
                                        val t = Temp.newtemp () 
                                        val {name, type_} = x
                                    in
                                        (table := Symbol.enter (!table, name, T.TEMP t));
                                        (T.TEMP t)::(arguments xs)
                                    end
                        in
                            arguments args
                        end
                in
                    (* make an entry of the function *)
                    (funcDecTable := Symbol.enter (!funcDecTable, name, T.LABEL funcLabel));
                    (* make an entry of the function and its args, this is where the caller will pass its
                    arguments *)
                    (funcTable := Symbol.enter (!funcTable, name, args_regs));
                    let
                        val body_trans = translateExp (body)
                    in
                        seq ([
                            T.JUMP (T.NAME jumpfunct, [jumpfunct]), (* we dont want to skip the function if its in the begining. *)
                            T.LABEL funcLabel, 
                            T.MOVE (T.TEMP retreg, ReturnAddressRegister),
                            T.MOVE (ReturnValueRegister, body_trans),
                            T.JUMP (T.TEMP retreg, []),
                            T.LABEL jumpfunct
                        ])
                    end
                end
            |   _ => raise Unimplemented "Declaration")

    and translateDecs_ ([]) = [nop]
        | translateDecs_ ((x::xs)) = (translateDec (x)) 
                                        :: (translateDecs_ (xs))

    and translateDecs ([]) = nop
        | translateDecs (ls) = seq (translateDecs_ (ls))

    and translateExps_ ([]) = T.CONST 0
        | translateExps_ ((x::xs)) =
            T.ESEQ (T.EXP (translateExp (x)), (translateExps_ (xs)))

    and translateExps ([]) = T.CONST 0
        | translateExps (ls) = translateExps_ (ls)

    and translateLvalue (lval) =
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

    and translateConditionExp (exp) =
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
                    |   A.Gt => RelOp T.GT
                    |   A.Lt => RelOp T.LT
                    |   A.Ge => RelOp T.GE
                    |   A.Le => RelOp T.LE
                    |   _ => raise SyntaxError)
                in
                    case oper' of
                        RelOp oper => 
                            (fn (succ, fail) => T.CJUMP (oper, translateExp (left), translateExp (right), succ, fail))
                        | _ => raise TranslationError
                end
            | A.SeqExp exps =>
                let
                    val len = List.length exps
                    val last = List.nth (exps, len - 1)
                in
                    translateConditionExp (last)
                end
            | _ => raise SyntaxError

    and translateExp (exp) = 
        (case exp of
            A.NilExp => T.CONST 0
        |   A.IntExp n => T.CONST n
        |   A.StringExp _ => raise Unimplemented "Strings"
        |   A.ArrayExp _ => raise Unimplemented "Arrays"
        |   A.RecordExp _ => raise Unimplemented "Records"
        |   A.New _ => raise Unimplemented "Object oriented programming."
        |   A.LetExp {decs, body} => 
                let
                    val decs_trans = translateDecs (decs)
                    val body_trans = translateExps (body)
                in
                    T.ESEQ (decs_trans, body_trans)
                end
        |   A.LvalExp lval => 
                translateLvalue (lval) 
        |   A.FunctionCall {name, args} =>
                let
                    val ret = Temp.newlabel ()
                    val prevret = Temp.newtemp ()
                    fun merge (a::as_) (b::bs_) = (a, b)::(merge as_ bs_)
                        | merge [] [] = []
                        | merge _ _ = raise SyntaxError
                    
                    fun args_trans (arg::xs) =
                        let
                            val arg_trans = translateExp (arg)
                        in
                            (arg_trans)::(args_trans xs)
                        end
                        | args_trans [] = []
                    
                    fun move_args ((reg, arg)::xs)=
                        let
                            val arg_trans = translateExp (arg)
                        in
                            (T.MOVE (reg, arg_trans))::(move_args xs)
                        end
                        | move_args [] = []
                in
                    case (Symbol.look (!funcDecTable, name), Symbol.look (!funcTable, name)) of
                        (SOME (T.LABEL flabel), SOME fargs) => 
                            T.ESEQ (
                                seq (
                                    move_args (merge fargs args) @
                                    [T.MOVE (T.TEMP prevret, ReturnAddressRegister),
                                    T.MOVE (ReturnAddressRegister, T.NAME ret),
                                    T.EXP (T.CALL (T.NAME flabel, args_trans args)),
                                    T.LABEL ret,
                                    T.MOVE (ReturnAddressRegister, T.TEMP prevret)
                                    ]
                                ), ReturnValueRegister
                            )
                        | (_, _) => raise UndefinedVariable (Symbol.name name)
                end
        |   A.Negate exp =>
                let
                    val exp_trans = translateExp (exp)
                in
                    T.BINOP (T.MINUS, T.CONST 0, exp_trans)
                end
        |   A.OpExp {left, oper, right} =>
                let
                    val left_trans = translateExp (left)
                    val right_trans = translateExp (right)
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
        |   A.SeqExp exps => translateExps (exps)
        |   A.AssignExp {object, exp} =>
                let
                    val lval = translateLvalue (object)
                    val exp = translateExp (exp)
                in
                    unEx (Nx (T.MOVE (lval, exp)))
                end
        |   A.IfElseExp {cond, succ, fail} =>
                let
                    val t = Temp.newlabel ()
                    val f = Temp.newlabel ()
                    val c = Temp.newlabel ()
                    val r = Temp.newtemp()
                    val cond_trans = translateConditionExp (cond)
                    val succ_trans = translateExp (succ)
                    val fail_trans = (case fail of
                                        NONE => NONE
                                        | SOME e => 
                                            let
                                                val fail_trans = translateExp (e)
                                            in
                                                SOME fail_trans
                                            end)
                in
                    case fail_trans of
                        NONE =>
                            T.ESEQ (
                                seq([
                                    cond_trans (t, c),
                                    T.LABEL t,
                                    unNx (Ex succ_trans),
                                    T.LABEL c
                                ]), T.CONST 0
                            )
                     | SOME trans =>
                            T.ESEQ (
                                seq([
                                    cond_trans (t, f),
                                    T.LABEL t,
                                    T.MOVE (T.TEMP r, succ_trans),
                                    T.JUMP (T.NAME c, [c]),
                                    T.LABEL f,
                                    T.MOVE (T.TEMP r, trans),
                                    T.LABEL c
                                ]), T.TEMP r
                            )
                end
        |   A.WhileExp {cond, body} =>
                let
                    val test = Temp.newlabel ()
                    val done = Temp.newlabel ()
                    val continue = Temp.newlabel ()
                    val cond_trans = translateConditionExp (cond)
                    val body_trans = translateExp (body)
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
                    val init_exp = translateExp (exp)
                    val cond_trans = translateExp (exit_cond)
                in
                    table := Symbol.enter (!table, name, T.TEMP t);
                    let
                        val body_trans = translateExp (body)
                    in
                        T.ESEQ (
                            seq ([
                                T.MOVE (T.TEMP t, init_exp),
                                T.LABEL test,
                                T.CJUMP (T.LT, T.TEMP t, cond_trans, continue, done),
                                T.LABEL continue,
                                T.MOVE (T.TEMP t, T.BINOP (T.PLUS, T.TEMP t, T.CONST 1)),
                                unNx (Ex body_trans),
                                T.JUMP (T.NAME test, [test]),
                                T.LABEL done
                            ]), T.CONST 0
                        )
                    end
                end
        |   A.BreakExp => raise Unimplemented "break"
        |   _ => raise Unimplemented "Expression" 
        handle Unimplemented x => (print ("Unimplemented Translation: " ^ x ^ "\n"); OS.Process.exit OS.Process.failure)
        handle UndefinedVariable x => (print ("Undefined variable: " ^ x ^ "\n"); OS.Process.exit OS.Process.failure)
        )

end