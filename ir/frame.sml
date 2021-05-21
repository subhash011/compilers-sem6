signature FRAME =
sig
    type frame
    datatype access = InReg of Temp.temp
                | InFrame of int
    val newFrame: {name: Temp.label, args: bool list} -> frame
    val name: frame -> Temp.label
    val args: frame -> access list
    val allocLocal: frame -> bool -> access
    val getAccessExp: access -> Tree.exp -> Tree.exp
    val ws: int
    val FP: Temp.temp
    val SP: Temp.temp
    val RA: Temp.temp
    val RV: Temp.temp
end

structure Frame: FRAME =
struct
    val ws = 8
    structure T = Tree
    datatype access = InReg of Temp.temp
                    | InFrame of int

    
    val SP = Temp.newtemp ()
    val FP = Temp.newtemp ()
    val RA = Temp.newtemp ()
    val RV = Temp.newtemp ()

    type frame = {
        name: Temp.label,
        args: access list, 
        locals: int ref
    }
    
    fun name {name, args, locals} = name

    fun args {name, args, locals} = args

    fun newFrame {name, args} =
        let
            fun allocateArgs ([], _) = []
                | allocateArgs (escapes::xs, fpoffset) =
                if escapes
                then (InFrame fpoffset)::(allocateArgs (xs, fpoffset + ws))
                else (InReg (Temp.newtemp ()))::(allocateArgs (xs, fpoffset))
            
            (* skip return address *)
            val access = allocateArgs (args, 2 * ws)
        in
            {name = name, args = access, locals = ref 0}
        end

    fun allocLocal {name, args, locals} escapes =
        if escapes
        then (locals := !locals + 1; InFrame (!locals * ws))
        else InReg (Temp.newtemp ())

    fun getAccessExp (InFrame fpoffset) = (fn fp => T.MEM (T.BINOP (T.MINUS, fp, T.CONST fpoffset)))
        | getAccessExp (InReg reg) = (fn _ => T.TEMP reg)

end