signature CANON = 
sig
    val linearize: Tree.stm -> Tree.stm list
    val basicBlocks: Tree.stm list -> (Tree.stm list list * Temp.label)
    val traceSchedule: Tree.stm list list * Temp.label -> Tree.stm list
end

structure Canon: CANON =
struct
    structure T = Tree
    
    fun linearize stm = 
        let
            val nop = T.EXP (T.CONST 0)

            infix %
            fun (T.EXP(T.CONST _)) % x = x
                | x % (T.EXP(T.CONST _)) = x
                | x % y = T.SEQ(x,y)

            and commute (T.EXP (T.CONST _), _) = true
                | commute (_, T.NAME _) = true
                | commute (_, T.CONST _) = true
                | commute _ = false
            
            and reorder_stm (l, build) =    
                let
                    val (s1, l1) = reorder l
                in
                    s1 % (build l1)
                end
            
            and do_stm (T.MOVE (T.TEMP t, T.CALL (e1, e2))) = 
                    reorder_stm (e1::e2, fn e1::e2 => T.MOVE (T.TEMP t, T.CALL (e1, e2)) | _ => nop)
                | do_stm (T.MOVE (T.TEMP t, b)) = 
                    reorder_stm ([b], fn [b] => T.MOVE(T.TEMP t, b) | _ => nop)
                | do_stm (T.MOVE (T.MEM e, b)) = 
                    reorder_stm ([e, b], fn [e, b] => T.MOVE(T.MEM e, b) | _ => nop)
                | do_stm (T.EXP (T.CALL(e1, e2))) = 
                    reorder_stm (e1::e2, fn e1::e2 => T.EXP(T.CALL(e1, e2)) | _ => nop)
                | do_stm (T.EXP e) = 
                    reorder_stm ([e], fn [e] => T.EXP e | _ => nop)
                | do_stm (T.JUMP (e, labs)) = 
                    reorder_stm ([e], fn [e] => T.JUMP (e, labs) | _ => nop)
                | do_stm (T.CJUMP (p, a, b, t, f)) = 
                    reorder_stm ([a, b], fn [a, b] => T.CJUMP (p, a, b, t, f) | _ => nop)
                | do_stm (T.SEQ (a, b)) = (do_stm a) % (do_stm b)
                | do_stm s = reorder_stm ([], fn _ => s)

            and reorder_exp (l, build) =    
                let
                    val (s1, l1) = reorder l
                in
                    (s1, build l1)
                end
            
            and do_exp (T.BINOP (p, a, b)) = 
                    reorder_exp ([a, b], fn [a, b] => T.BINOP (p, a, b) | _ => T.CONST 0)
                | do_exp (T.MEM (a)) = 
                    reorder_exp([a], fn [a] => T.MEM (a) | _ => T.CONST 0)
                | do_exp (T.CALL (e1, e2)) = 
                    reorder_exp (e1::e2, fn e1::e2 => T.CALL (e1, e2) | _ => T.CONST 0)
                | do_exp (T.ESEQ (s, e)) =  
                    let
                        val stmts1 = do_stm s
                        val (stmts2, e) = do_exp e
                    in
                        (stmts1 % stmts2, e)
                    end
                | do_exp e = reorder_exp ([], fn _ => e)

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
            
            and linear (T.SEQ (a, b), l) = linear (a, linear (b, l))
                | linear (s, l) = s::l
        in
            linear (do_stm stm, nil)
        end

        fun basicBlocks stmts = 
            let
                val done = Temp.newlabel()
                fun makeBlocks ((head as T.LABEL _)::tail, blocks) =
                        let
                            fun makeBlock ((s as T.JUMP _)::rest, block) = 
                                    endBlock (rest, s::block)
                                | makeBlock ((s as T.CJUMP _)::rest, block) = 
                                    endBlock (rest, s::block)
                                | makeBlock ((s as T.LABEL label)::rest, block) = 
                                    makeBlock (T.JUMP (T.NAME label, [label])::s::rest, block)
                                | makeBlock (s::rest, block) = 
                                    makeBlock (rest, s::block)
                                | makeBlock (nil, block) = 
                                    makeBlock ([T.JUMP (T.NAME done, [done])], block)

                            and endBlock (stmts, block) = makeBlocks (stmts, (rev block)::blocks)
                        in
                            makeBlock (tail, [head])
                        end
                    | makeBlocks (nil, blocks) = rev blocks
                    | makeBlocks (stmts, blocks) = 
                        let
                            val label = T.LABEL(Temp.newlabel())
                        in
                            makeBlocks(label::stmts, blocks)
                        end

            in
                (makeBlocks (stmts, nil), done)
            end
end