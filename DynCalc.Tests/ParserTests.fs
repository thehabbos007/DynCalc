module ParserTests

open Xunit
open FsCheck.Xunit

open DynCalc.Lang.Ast
open DynCalc.Lang.Parser


// [<Fact>]
// let ``Parser Tests`` () =
//     [ ("var=1;", Assignment("var", ExprAssignment(Number 1.)))
//       ("var=2+2;", Assignment("var", ExprAssignment(Binary(Number 2., Plus, Number 2.))))
//       ("var=A2+4;", Assignment("var", ExprAssignment(Binary(Variable("A2"), Plus, Number 4.))))
//       ("var=4*A1+2/2-1;",
//        Assignment(
//            "var",
//            ExprAssignment(
//                Binary(
//                    Binary(Binary(Number 4., Multiply, Variable("A1")), Plus, Binary(Number 2., Divide, Number 2.)),
//                    Minus,
//                    Number 1.
//                )
//            )
//        ))
//       ("var=A1+A2+S7;",
//        Assignment("var", ExprAssignment(Binary(Binary(Variable("A1"), Plus, Variable("A2")), Plus, Variable("S7"))))) ]
//     |> List.forall (fun (text, result) -> unwrapParse text = result)
//     |> Assert.True



type LimitedRefereceCol =
    | A
    | B
    | C
    | D
    | E
    override self.ToString() =
        match self with
        | A -> "A"
        | B -> "B"
        | C -> "C"
        | D -> "D"
        | E -> "E"

type LimitedBuiltins =
    | Fun1
    | Fun2
    | Fun3
    | Fun4
    | Fun5
    override self.ToString() =
        match self with
        | Fun1 -> "fun1"
        | Fun2 -> "fun2"
        | Fun3 -> "fun3"
        | Fun4 -> "fun4"
        | Fun5 -> "fun5"

type LimitedBuiltinArgs =
    | Val1
    | Val2
    | Val3
    | Val4
    | Val5
    override self.ToString() =
        match self with
        | Val1 -> "Val1"
        | Val2 -> "Val2"
        | Val3 -> "Val3"
        | Val4 -> "Val4"
        | Val5 -> "Val5"


// let rec stringifyAstRefs (expr: Assignment<LimitedRefereceCol, LimitedBuiltins, LimitedBuiltinArgs>) =
//     match expr with
//     | FunctionCall (id, args) -> FunctionCall($"{id}", List.map (sprintf "%A") args)
//     | Binary (l, o, r) -> Binary(stringifyAstRefs l, o, stringifyAstRefs r)
//     | Number x -> Number x

// [<Property>]
// let ``parser equivalance`` (exp: Assignment<LimitedRefereceCol, LimitedBuiltins, LimitedBuiltinArgs>) =
//     (stringifyAstRefs exp) = unwrapParse (toString exp)
