signature ENV =
sig
    datatype enventry = VarEntry of {access: Frame.access, type_: Symbol.symbol option}
                      | FunEntry of {
                            frame: Frame.frame, 
                            label: Temp.label, 
                            args: {name: Symbol.symbol, type_: Symbol.symbol} list, 
                            ret: Symbol.symbol option
                        }
    
    val base_venv: enventry Symbol.table
end

structure Env: ENV =
struct

    datatype enventry = VarEntry of {access: Frame.access, type_: Symbol.symbol option}
                      | FunEntry of {
                            frame: Frame.frame, 
                            label: Temp.label, 
                            args: {name: Symbol.symbol, type_: Symbol.symbol} list, 
                            ret: Symbol.symbol option
                        }
    
    (* This can be modified to add inbuilt functions. *)
    val base_venv = Symbol.empty

end

