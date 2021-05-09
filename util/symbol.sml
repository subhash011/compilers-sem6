(* This file is to help store the variables *)

signature TABLE =
sig
    type key

    type 'a table
    val empty       : 'a table
    val enter       : 'a table * key * 'a -> 'a table
    val look        : 'a table * key -> 'a option
    val find        : 'a table * key -> 'a option
    val remove      : 'a table * key -> 'a table * 'a
    val numItems    : 'a table -> int
    val listKeys    : 'a table -> int list
    val listItems   : 'a table -> 'a list
    val listItemsi  : 'a table -> (int * 'a) list
    val map         : ('a -> 'b) -> 'a table -> 'b table
end


functor IntMapTable (type key val getInt: key -> int) : TABLE =
struct

    type key = key

    structure H = RedBlackMapFn (struct
                                type ord_key = int
                                val compare = Int.compare
                                end)

    type 'a table = 'a H.map
    val empty = H.empty
    fun enter (t, k, a) = H.insert(t, getInt k, a)
    fun look(t, k) = H.find(t, getInt k)
    val find = look
    fun remove(t, k) = H.remove(t, getInt k)
    fun numItems(t) = H.numItems(t)
    fun listKeys (t) = H.listKeys (t)
    fun listItems (t) = H.listItems (t)
    fun listItemsi (t) = H.listItemsi (t)
    fun map(f) = H.map (f)
end

signature SYMBOL =
sig
    eqtype symbol
    val symbol: string -> symbol
    val name: symbol -> string
    type 'a table
    val empty: 'a table
    val enter: 'a table * symbol * 'a -> 'a table
    val look: 'a table * symbol -> 'a option
    val remove: 'a table * symbol -> 'a table * 'a 
end

structure Symbol :> SYMBOL =
struct

  type symbol = string * int

  structure H = HashTable

  exception Symbol
  val nextsym = ref 0
  val sizeHint = 128
  val hashtable : (string,int) H.hash_table =
		H.mkTable(HashString.hashString, op = ) (sizeHint, Symbol)

  fun symbol name =
      case H.find hashtable name
       of SOME i => (name,i)
        | NONE => 
            let 
                val i = !nextsym
	        in 
                nextsym := i+1;
		        H.insert hashtable (name, i);
		        (name,i)
		    end

  fun name(s,n) = s

  structure Table = IntMapTable(type key = symbol 
                                fun getInt(s,n) = n)

  type 'a table= 'a Table.table
  val empty = Table.empty
  val enter = Table.enter
  val look = Table.look
  val remove = Table.remove
end