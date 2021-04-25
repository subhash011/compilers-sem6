signature CANON = 
sig
    val linearize: Tree.stm -> Tree.stm list
    val basicBlocks: Tree.stm list -> (Tree.stm list list * Temp.label)
    val traceSchedule: Tree.stm list list * Temp.label -> Tree.stm list
end

structure Canon: CANON =
struct
    
end