signature TEMP = 
sig
    type temp
    val newtemp : unit -> temp
    structure Table : TABLE sharing type Table.key = temp
    type label = Symbol.symbol
    val newlabel : unit -> label
    val namedlabel : string -> label
end

structure Temp : TEMP = struct
    type temp = int
    type label = Symbol.symbol
    val temps = ref 0
    val labels = ref 0
    val namedlabel = Symbol.symbol
    structure Table = IntMapTable (type key = int fun getInt n = n)
    fun incRef refVar = let val tmp = !refVar in ( refVar := tmp + 1; tmp ) end
    fun newtemp () = incRef temps
    fun newlabel () = Symbol.symbol ("L" ^ Int.toString (incRef labels) )

end