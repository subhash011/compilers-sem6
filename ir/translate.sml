signature TRANSLATE =
sig
    datatype exp =  Ex of Tree.exp
                    | Nx of Tree.stm
                    | Cx of Temp.label * Temp.label -> Tree.stm

    val unEx: exp -> Tree.exp
    val unNx: exp -> Tree.stm
    val unCx: exp -> Temp.label * Temp.label -> Tree.stm
    val seq: Tree.stm list -> Tree.stm
end

structure Translate: TRANSLATE =
struct

    exception TranslationError
    exception NotWellTyped

    structure T = Tree

    datatype exp =  Ex of Tree.exp
                    | Nx of Tree.stm
                    | Cx of Temp.label * Temp.label -> Tree.stm

    (* Generates nested SEQ's which is cleaned up by linear function in canon.sml *)
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
                (* If none, compare e and 1 and jump accordingly *)
                T.CONST 1 => (fn (succ, fail) => T.JUMP (T.NAME succ, [succ]))
                (* If e is 0, jump to failure label *)
                | T.CONST 0 => (fn (succ, fail) => T.JUMP (T.NAME fail, [fail]))
                (* If none, compare e and 1 and jump accordingly *)
                | _ => (fn (succ, fail) => T.CJUMP (T.EQ, e, T.CONST 1, succ, fail)))
        | unCx (Cx func) = func
        | unCx (e as Nx s) = unCx (Ex (unEx e))


end