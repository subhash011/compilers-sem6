(*

AST of the Tiger Language

*)

structure Tiger =
struct

    type id = Symbol.symbol
    type TypeId = id
    type TyFields = {var: id, type_: TypeId} list

    datatype ast = Expr of Exp
            | Decs of Dec list

    and Exp = Nil
            | Int of int
            | String of string
            | Array of {type_: TypeId, length: Exp, init: Exp}
            | Record of {type_: TypeId, init: {var: id, value: Exp} list}
            | New of TypeId
            | Lval of Lvalue
            | FunctionCall of {id: id, args: Exp list}
            | MethodCall of {object: Lvalue, id: id, args: Exp list}
            | Negate of Exp
            | Op of {left: Exp, oper: BinOp, right: Exp}
            | Exps of Exp list
            | Assignment of {object: Lvalue, exp: Exp}
            | IfElse of {cond: Exp, succ: Exp, fail: Exp option}
            | While of {cond: Exp, body: Exp}
            | For of {init: {var: id, exp: Exp}, exit_cond: Exp, body: Exp}
            | Break
            | Let of {decs: Dec list, body: Exp list}

    and Lvalue = Variable of id
            | Reference of {object: Lvalue, var: id}
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

    and Dec = TypeDec of {id: id, type_: Type}
            | ClassDec of {id: id, extends: TypeId option, classfields: ClassFields}
            | VarDec of VarDecType
            | FunDec of FunDecType
            | PrimitiveDec of FunDecType

    and ClassField = AttrDec of VarDecType
            | MethodDec of FunDecType

    and Type = TypeAlias of TypeId
            | RecordType of TyFields
            | ArrayType of TypeId
            | ClassType of {extends: TypeId option, classfields: ClassFields}

    withtype ClassFields = ClassField list
    and VarDecType = {id: id, type_: TypeId option, init: Exp}
    and FunDecType = {id: id, args: TyFields, ret: TypeId option, body: Exp}
end

(*

========================================
let
    type int_array := array of int
    var  table := int_array[100] of 0
in
    print(table[0])
end
========================================

Aim: To convert the above syntax into ast step by step.

ast
Expr (Exp)
Expr (Let {decs: Dec List, body: Exp list})

******************************************************
1. type int_array := array of int
TypeDec {id = "int_array", type_ = ArrayType "int"}

2. int_array[100] of 0
Array {type_ = "int_array", length = Int 100, init = Int 0}

3. var  table := int_array[100] of 0
VarDec {id = "table", type_ = SOME "var", init = Array {type_ = "int_array", length = Int 100, init = Int 0}}

******************************************************

So decs = [TypeDec {id = "int_array", type_ = ArrayType "int"}, VarDec {id = "table", type_ = SOME "var", init = Array {type_ = "int_array", length = Int 100, init = Int 0}}]

******************************************************
1. table[0]
Lval (ArrayAccess {object = Variable "table", index = Int 0})]}

2. print(table[0]) (* Assuming print is already defined elswhere *)
FunctionCall {id = "print", args = [Lval (ArrayAccess {object = Variable "table", index = Int 0})]}

******************************************************

So exps = [FunctionCall {id = "print", args = [Lval (ArrayAccess {object = Variable "table", index = Int 0})]}]

So finally,
Run the following 4 lines to see the final output as AST of Tiger.

open Tiger;
val decs = [TypeDec {id = "int_array", type_ = ArrayType "int"}, VarDec {id = "table", type_ = SOME "var", init = Array {type_ = "int_array", length = Int 100, init = Int 0}}];
val exps = [FunctionCall {id = "print", args = [Lval (ArrayAccess {object = Variable "table", index = Int 0})]}];
val program = Expr (Let {decs=decs, body = exps});

*)