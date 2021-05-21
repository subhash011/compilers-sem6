signature CANON = 
sig
    val linearize: Tree.stm -> Tree.stm list
    val basicBlocks: Tree.stm list -> (Tree.stm list list * Temp.label)
    val traceSchedule: Tree.stm list list * Temp.label -> Tree.stm list
end

structure Canon: CANON =
struct

    exception CanonisationError

    structure T = Tree
    
    fun linearize stm = 
        let
            val nop = T.EXP (T.CONST 0)

            (* This function shortens the SEQ expression by remove unnecessary
               sub-expressions. *)
            infix %
            fun (T.EXP(T.CONST _)) % x = x
                | x % (T.EXP(T.CONST _)) = x
                | x % y = T.SEQ(x,y)

            (* All the trivial statemens commute. *)
            and commute (stmt, exp) = 
                case (stmt, exp) of
                    (T.EXP (T.CONST _), _) => true
                    | (_, T.NAME _) => true
                    | (_, T.CONST _) => true
                    | _ => false
            
            and reorder_stm (l, build) =    
                let
                    val (s1, l1) = reorder l
                in
                    s1 % (build l1)
                end
            
            and do_stm stmt = 
                case stmt of
                    (T.MOVE (T.TEMP t, T.CALL (e1, e2))) => 
                        reorder_stm (e1::e2, fn e1::e2 => T.MOVE (T.TEMP t, T.CALL (e1, e2)) | _ => nop)
                    | (T.MOVE (T.TEMP t, b)) => 
                        reorder_stm ([b], fn [b] => T.MOVE(T.TEMP t, b) | _ => nop)
                    | (T.MOVE (T.MEM e, b)) =>
                        reorder_stm ([e, b], fn [e, b] => T.MOVE(T.MEM e, b) | _ => nop)
                    | (T.EXP (T.CALL(e1, e2))) => 
                        reorder_stm (e1::e2, fn e1::e2 => T.EXP(T.CALL(e1, e2)) | _ => nop)
                    | (T.EXP e) => 
                        reorder_stm ([e], fn [e] => T.EXP e | _ => nop)
                    | (T.JUMP (e, labs)) =>
                        reorder_stm ([e], fn [e] => T.JUMP (e, labs) | _ => nop)
                    | (T.CJUMP (p, a, b, t, f)) =>
                        reorder_stm ([a, b], fn [a, b] => T.CJUMP (p, a, b, t, f) | _ => nop)
                    | (T.SEQ (a, b)) => (do_stm a) % (do_stm b)
                    | s => reorder_stm ([], fn _ => s)

            and reorder_exp (l, build) =    
                let
                    val (s1, l1) = reorder l
                in
                    (s1, build l1)
                end
            
            and do_exp exp = 
                case exp of
                    (T.BINOP (p, a, b)) =>
                        reorder_exp ([a, b], fn [a, b] => T.BINOP (p, a, b) | _ => T.CONST 0)
                    | (T.MEM (a)) =>
                        reorder_exp([a], fn [a] => T.MEM (a) | _ => T.CONST 0)
                    | (T.CALL (e1, e2)) => 
                        reorder_exp (e1::e2, fn e1::e2 => T.CALL (e1, e2) | _ => T.CONST 0)
                    | (T.ESEQ (s, e)) =>
                        let
                            val stmts1 = do_stm s
                            val (stmts2, e) = do_exp e
                        in
                            (stmts1 % stmts2, e)
                        end
                    | e => reorder_exp ([], fn _ => e)

            (*
                reorder function makes sure CALL's return value is
                stored in a register so that consecutive calls don't overwrite
                each other's result.
            *)
            and reorder ((T.CALL expr )::rest) =    
                    let
                        val e = T.CALL expr
                        val t = Temp.newtemp()
                    in
                        reorder (T.ESEQ(T.MOVE(T.TEMP t, e), T.TEMP t) :: rest)
                    end
                | reorder (a :: rest) = 
                    let
                        val (stmts, e) = do_exp a
                        val (stmts1, e1) = reorder rest 
                    in
                        if commute (stmts1, e)
                        then (stmts % stmts1, e :: e1)
                        else    
                            let
                                val t = Temp.newtemp()
                            in
                                (stmts % T.MOVE (T.TEMP t, e) % stmts1, (T.TEMP t) :: e1)
                            end
                    end
                | reorder nil = (nop, nil)
            
            (* 
                Linearised expression might look like
                SEQ (a, SEQ (b, ....)), this function converts
                it to a list which looks like [a, b, ....]
            *)
            and linear (T.SEQ (a, b), l) = linear (a, linear (b, l))
                | linear (s, l) = s::l
        in
            linear (do_stm stm, nil)
        end

    fun basicBlocks stmts = 
        let
            
            val done = Temp.newlabel() (* Special label for the last block. *)

            (*
                A block would look like [LABEL ... statements .... (JUMP/CJUMP)] and
                the last block will be the label done.
            *)
            fun makeBlocks (stmts, block, blocks) = 
                case stmts of
                    (* If JUMP or CJUMP, end current block and start new one. *)
                    ((s as T.JUMP _)::rest) => 
                        makeBlocks (rest, [], (List.rev (s::block))::blocks)
                    | ((s as T.CJUMP _)::rest) =>
                        makeBlocks (rest, [], (List.rev (s::block))::blocks)
                    
                    (* If LABEL, append JUMP instruction to the current block and start a
                    new block. *)
                    | ((s as T.LABEL label)::rest) =>
                        let
                            val jmp = T.JUMP (T.NAME label, [label])
                        in
                            case block of
                                nil => makeBlocks (rest, [s], blocks)
                                | _ => makeBlocks (rest, [s], (List.rev (jmp::block))::blocks)
                        end
                    (* If its neither a label or a branch instruction, add it current block
                    and continue. *)
                    | (s::rest) => makeBlocks (rest, s::block, blocks)
                    (* If there are no more statements, add jump instruction to done label,
                    reverse the list and return it. *)
                    | [] => let
                                val jmp = T.JUMP (T.NAME done, [done])
                            in
                                List.rev (List.rev (jmp::block)::blocks)
                            end
        in
            case stmts of
                (* If we see label, start making blocks *)
                ((s as T.LABEL _)::rest) => (makeBlocks (rest, [s], nil), done)
                (* If we don't see a label, add a new label and start. *)
                | (s::rest) => 
                    let
                        val label = T.LABEL (Temp.newlabel())
                    in
                        (makeBlocks (stmts, [label], nil), done)
                    end
                (* If no statements, return done label. *)
                | nil => (makeBlocks (nil, nil, nil), done)
                    
                
        end
    
    fun traceSchedule (blocks, done) = 
        let
            fun addBlocksToTable blocks = List.foldr addBlockToTable Symbol.empty blocks
            (* Add the given block to the Map in Symbol. *)
            and addBlockToTable (block, table) =
                case block of
                    (block as (T.LABEL label::_)) => Symbol.enter (table, label, block)
                    | _ => table
            
            fun splitList l =
                let
                    val lastIndex = List.length l - 1
                    val first = List.take (l, lastIndex)
                    val last = List.nth (l, lastIndex)
                in
                    (first, last)
                end
            
            fun runTrace (block as (T.LABEL label::_), rest, table) = 
                let
                    val table = Symbol.enter (table, label, nil)
                in
                    case splitList block of
                        (* If the last instruction is JUMP and if the block to be jumped to
                        is not yet seen, append here to reduce JUMP instructions. *)
                        (first, T.JUMP (T.NAME label, _)) =>
                            (case (Symbol.look (table, label)) of
                                SOME (next as _::_) => first @ runTrace (next, rest, table)
                                | _ => block @ pickBlock (rest, table))
                        (* If the last instruction is CJUMP try adding the false block after
                        the current block. *)
                        | (first, T.CJUMP (oper, a, b, succ, fail)) => 
                            (case (Symbol.look (table, succ), Symbol.look (table, fail)) of
                                (_, SOME (fb as _::_)) => block @ runTrace (fb, rest, table)
                                | (SOME (sb as _::_), _) =>
                                    let
                                        val cjmp = T.CJUMP(T.notRel oper, a, b, fail, succ)
                                    in
                                        first @ [cjmp] @ runTrace (sb, rest, table)
                                    end
                                | _ => 
                                    let
                                        val l = Temp.newlabel()
                                        val cjmp = T.CJUMP(oper, a, b, succ, l)
                                    in
                                        first @ [cjmp, T.LABEL l, T.JUMP (T.NAME fail, [fail])]
                                                @ pickBlock (rest, table)
                                    end)
                        | (first, T.JUMP _) => block @ pickBlock (rest, table)
                        | _ => raise CanonisationError
                end
                | runTrace _ = raise CanonisationError

            (* Pick a block to perform trace on and return the trace. *)
            and pickBlock (block::rest, table) = 
                (case block of
                    (T.LABEL label::_) =>
                        (case Symbol.look (table, label) of
                            SOME (block as _::_) => runTrace (block, rest, table)
                            | _ => pickBlock (rest, table))
                    | nil => nil
                    | _ => raise CanonisationError)
                | pickBlock (nil, table) = nil
            val table = addBlocksToTable blocks
            val trace = pickBlock (blocks, table) @ [T.LABEL done]
            (*  Remove statements with no side effects in the trace
                since they don't mean anything. *)
            fun removeUnwanted [] = []
                | removeUnwanted (x::xs) =
                    (case x of
                        T.EXP _ => removeUnwanted xs
                    |   _ => x::(removeUnwanted xs))
        in
            removeUnwanted trace
        end
end