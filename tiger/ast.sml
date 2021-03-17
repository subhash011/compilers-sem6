(*

AST of the Tiger Language

*)

structure Tiger =
struct

    type id = Symbol.symbol
    type TypeId = id
    type TyFields = {name: id, type_: TypeId} list

    datatype ast = Expr of Exp
            | Decs of Dec list

    and Exp = Nil
            | Int of int
            | String of string
            | Array of {type_: TypeId, length: Exp, init: Exp}
            | Record of {type_: TypeId, init: {name: id, value: Exp} list}
            | New of TypeId
            | LvalExp of Lvalue
            | FunctionCall of {name: id, args: Exp list}
            | MethodCall of {object: Lvalue, name: id, args: Exp list}
            | Negate of Exp
            | Op of {left: Exp, oper: BinOp, right: Exp}
            | Exps of Exp list
            | Assignment of {object: Lvalue, exp: Exp}
            | IfElse of {cond: Exp, succ: Exp, fail: Exp option}
            | While of {cond: Exp, body: Exp}
            | For of {init: {name: id, exp: Exp}, exit_cond: Exp, body: Exp}
            | Break
            | Let of {decs: Dec list, body: Exp list}

    and Lvalue = Variable of id
            | Reference of {object: Lvalue, name: id}
            | ArrayAccess of {object: Lvalue, index: Exp}

    and BinOp = Plus
            | Minus
            | Mul
            | Div
            | Eq
            | Ne
            | Gt
            | Lt
            | Ge
            | Le
            | And
            | Or

    and Dec = TypeDec of {name: id, type_: Type}
            | ClassDec of {name: id, extends: TypeId option, classfields: ClassFields}
            | VarDec of VarDecType
            | FunDec of FunDecType
            | PrimitiveDec of {name: id, args: TyFields, ret: TypeId option}

    and ClassField = AttrDec of VarDecType
            | MethodDec of FunDecType

    and Type = TypeAlias of TypeId
            | RecordType of TyFields
            | ArrayType of TypeId
            | ClassType of {extends: TypeId option, classfields: ClassFields}

    withtype ClassFields = ClassField list
    and VarDecType = {name: id, type_: TypeId option, init: Exp}
    and FunDecType = {name: id, args: TyFields, ret: TypeId option, body: Exp}
end