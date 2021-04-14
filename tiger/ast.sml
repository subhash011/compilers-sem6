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

    and Exp = NilExp
            | IntExp of int
            | StringExp of string
            | ArrayExp of {type_: TypeId, length: Exp, init: Exp}
            | RecordExp of {type_: TypeId, init: {name: id, value: Exp} list}
            | New of TypeId
            | LvalExp of Lvalue
            | FunctionCall of {name: id, args: Exp list}
            | MethodCall of {object: Lvalue, name: id, args: Exp list}
            | Negate of Exp
            | OpExp of {left: Exp, oper: BinOp, right: Exp}
            | SeqExp of Exp list
            | AssignExp of {object: Lvalue, exp: Exp}
            | IfElseExp of {cond: Exp, succ: Exp, fail: Exp option}
            | WhileExp of {cond: Exp, body: Exp}
            | ForExp of {init: {name: id, exp: Exp}, exit_cond: Exp, body: Exp}
            | BreakExp
            | LetExp of {decs: Dec list, body: Exp list}

    and Lvalue = SimpleVar of id
            | FieldVar of {object: Lvalue, name: id}
            | SubscriptVar of {object: Lvalue, index: Exp}

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