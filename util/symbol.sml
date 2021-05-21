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
    val numItems: 'a table -> int
    val listKeys: 'a table -> int list
    val listItems: 'a table -> 'a list
    val listItemsi: 'a table -> (int * 'a) list
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
  val numItems = Table.numItems
  val listKeys = Table.listKeys
  val listItems = Table.listItems
  val listItemsi = Table.listItemsi
end