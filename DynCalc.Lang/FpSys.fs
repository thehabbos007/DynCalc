namespace DynCalc.Lang.FP


module Ast =
    type Symbol = string

    type AtomGeneric<'a> =
        | BoolAtom of bool
        | NumberAtom of 'a
        | SymbolAtom of Symbol

    type ObjectGeneric<'a> =
        | Bottom
        | DynamicVariable of string
        | AtomObject of AtomGeneric<'a>
        | SequenceObject of ObjectGeneric<'a> list

    type Object = ObjectGeneric<float>

    type Function =
        | Function of Symbol
        | Composition of Function * Function
        | Constant of Object
        | FunctionList of Function list
        | ApplyToAll of Function
        | BinaryToUnary of Function * Object
        | Reduce of Function
        | IfElse of Function * Function * Function
        | While of Function * Function

    type Definition = Definition of Symbol * Function

    type ExpressionGeneric<'a> =
        | Object of ObjectGeneric<'a>
        | Application of Function * ObjectGeneric<'a>

    type Expression = ExpressionGeneric<float>

    let atomToString =
        function
        | BoolAtom x -> x.ToString()
        | NumberAtom x -> x.ToString()
        | SymbolAtom x -> x.ToString()

    let rec objectToString =
        function
        | AtomObject x -> $"{atomToString x}"
        | SequenceObject xs -> $"""<{String.concat ", " <| List.map (objectToString) xs}>"""
        | DynamicVariable x -> x
        | Bottom -> "⊥"

    let boolify x = AtomObject <| BoolAtom x
    let numberfy x = AtomObject <| NumberAtom x


module Parser =
    open FParsec
    open Ast

    let number = pfloat |>> NumberAtom

    let symbolRegex = regex @"[A-Z]+"

    let symbol = symbolRegex |>> SymbolAtom

    let ptrue = stringReturn "T" true
    let pfalse = stringReturn "F" false
    let bool = (ptrue <|> pfalse) |>> BoolAtom

    let atom = (number <|> bool <|> symbol) |>> AtomObject

    let identifier = regex @"[a-z0-9_][A-Za-z0-9_]*"
    // Used for "variables" in application
    let variableIdentifier = regex @"[a-z_][A-Za-z0-9_]*"

    let betweenSame a x = between a a x
    let wsIgnore x = betweenSame spaces x

    let wrapped l r x =
        wsIgnore x |> between (pchar l) (pchar r)

    let sequence, sequenceImpl = createParserForwardedToRef ()


    let object =
        (attempt atom)
        <|> sequence
        <|> (variableIdentifier |>> DynamicVariable)

    let sequenceInner =
        between (pchar '<') (pchar '>')
        <| sepBy (object) (wsIgnore (pstring ","))
        |>> SequenceObject

    sequenceImpl := sequenceInner



    //    let term, termImpl = createParserForwardedToRef ()

    let func, funcImpl = createParserForwardedToRef ()

    let functionList =
        wrapped '[' ']'
        <| sepBy func (wsIgnore (pstring ","))

    let funcParser = new OperatorPrecedenceParser<Function, unit, unit>()

    funcParser.AddOperator(PrefixOperator("@", spaces, 2, true, ApplyToAll))
    funcParser.AddOperator(PrefixOperator("%", spaces, 2, true, Reduce))
    funcParser.AddOperator(InfixOperator(".", spaces, 1, Associativity.Right, (fun x y -> Composition(x, y))))


    let term =
        (functionList |>> FunctionList)
        <|> (pstring "while" >>. func .>>. func |>> While)
        <|> (pstring "if" >>. spaces >>. func
             .>> spaces
             .>> (pstring "->")
             .>> spaces
             .>>. func
             .>> spaces
             .>> pchar ';'
             .>> spaces
             .>>. func
             |>> (fun ((x, y), z) -> IfElse(x, y, z)))
        <|> ((pstring "bu" >>. spaces >>. func .>>. object)
             |>> BinaryToUnary)
        <|> ((pchar '#' >>. object) |>> Constant)
        <|> ((identifier <|> (many1Chars (anyOf "+-*/^")))
             |>> Function)
        <|> wrapped '(' ')' func


    funcParser.TermParser <- (term .>> spaces)

    funcImpl := funcParser.ExpressionParser

    let definition =
        wsIgnore (pstring "func") >>. identifier
        .>> spaces
        .>> (pchar '=')
        .>> spaces
        .>>. func
        .>> spaces
        |>> Definition


    let expression =
        attempt (
            func .>> spaces
            .>>. (pchar ':' >>. spaces >>. (attempt object))
            |>> Application
        )
        <|> (object |>> Object)

    let splitSelectArgument x =
        match run (pint32 .>>. opt (pchar 'r')) x with
        | Success ((idx, direction), _, _) -> Some(idx, direction.IsSome)
        | _ -> None

module Builtins =
    open Ast
    open Parser

    type EnvDefinition =
        | UserDefined of Function
        | Builtin of (Object -> Object)

    type Environment = Map<string, EnvDefinition>

    let empty = SequenceObject []

    let tl =
        function
        | SequenceObject [ _ ] -> empty
        | SequenceObject (_ :: tl) -> SequenceObject tl
        | _ -> Bottom

    let id x = x

    let atom =
        function
        | AtomObject _ -> boolify true
        | Bottom -> Bottom
        | _ -> boolify false

    let eq =
        function
        | SequenceObject [ x; y ] -> boolify (x = y)
        | _ -> Bottom

    let isEmpty =
        function
        | SequenceObject [] -> boolify true
        | Bottom -> Bottom
        | _ -> boolify false

    let reverse =
        function
        | SequenceObject x -> SequenceObject <| List.rev x
        | _ -> Bottom

    let distL =
        function
        | SequenceObject [ x; SequenceObject ys ] ->
            SequenceObject
            <| List.map (fun y -> SequenceObject [ x; y ]) ys
        | _ -> Bottom

    let distR =
        function
        | SequenceObject [ SequenceObject xs; y ] ->
            SequenceObject
            <| List.map (fun x -> SequenceObject [ x; y ]) xs
        | _ -> Bottom

    let length =
        function
        | SequenceObject x -> AtomObject(NumberAtom(List.length x |> float))
        | _ -> Bottom

    let trans =
        function
        | SequenceObject xs ->
            let transposed =
                xs
                |> List.map (function
                    | (SequenceObject x) -> x
                    | _ -> [])
                |> List.filter (fun x -> x <> [])
                |> FSharpx.Collections.List.transpose


            if List.forall (fun x -> x = []) transposed then
                []
            else
                transposed
                |> List.map (fun x -> SequenceObject(x))
            |> SequenceObject
        | _ -> Bottom


    let binOpMath op =
        function
        | SequenceObject [ AtomObject (NumberAtom x); AtomObject (NumberAtom y) ] -> numberfy (op x y)
        | _ -> Bottom

    let nAdd = binOpMath (+)
    let nSub = binOpMath (-)

    // (*) messes with my syntax highlighting :(
    let nMul = binOpMath (fun x y -> x * y)
    let nPow = binOpMath (fun x y -> x ** y)

    let nDiv =
        function
        | SequenceObject [ AtomObject (NumberAtom x); AtomObject (NumberAtom y) ] when y <> 0.0 -> numberfy (x / y)
        | _ -> Bottom


    let binOpBool op =
        function
        | SequenceObject [ AtomObject (BoolAtom x); AtomObject (BoolAtom y) ] -> boolify (op x y)
        | _ -> Bottom

    let bAnd = binOpBool (&&)
    let bOr = binOpBool (||)

    let bNot =
        function
        | AtomObject (BoolAtom x) -> boolify (not x)
        | _ -> Bottom

    let apndL =
        function
        | SequenceObject [ x; SequenceObject ys ] -> SequenceObject(x :: ys)
        | _ -> Bottom

    let apndR =
        function
        | SequenceObject [ SequenceObject xs; y ] -> SequenceObject(xs @ [ y ])
        | _ -> Bottom

    let selectL idx =
        function
        | SequenceObject xs ->
            match (List.indexed xs
                   |> List.tryFind (fun (i, _) -> i = (int idx)))
                with
            | Some (_, x) -> x
            | _ -> Bottom
        | _ -> Bottom

    let selectR idx =
        function
        | SequenceObject xs ->
            match (List.rev xs
                   |> List.indexed
                   |> List.tryFind (fun (i, _) -> i = (int idx)))
                with
            | Some (_, x) -> x
            | _ -> Bottom
        | _ -> Bottom


    // slow impl, doesn't matter in this PoC state
    let tlR =
        function
        | SequenceObject [ _ ] -> empty
        | SequenceObject xs ->
            match List.rev xs with
            | _ :: rest -> SequenceObject(List.rev rest)
            | _ -> SequenceObject []
        | _ -> Bottom


    let rotL =
        function
        | SequenceObject (x :: xs) -> SequenceObject(xs @ [ x ])
        | _ -> Bottom

    let rotR =
        function
        | SequenceObject ((_ :: _) as xs) ->
            let x = List.last xs
            let rest = List.rev xs |> List.tail |> List.rev
            SequenceObject(x :: rest)
        | _ -> Bottom


    let builtinEnvList =
        [ ("tl", tl)
          ("id", id)
          ("atom", atom)
          ("eq", eq)
          ("empty", isEmpty)
          ("reverse", reverse)
          ("distL", distL)
          ("distR", distR)
          ("length", length)
          ("+", nAdd)
          ("-", nSub)
          ("*", nMul)
          ("/", nDiv)
          ("^", nPow)
          ("trans", trans)
          ("and", bAnd)
          ("or", bOr)
          ("not", bNot)
          ("apndL", apndL)
          ("apndR", apndR)
          ("tlR", tlR)
          ("rotL", rotL)
          ("rotR", rotR) ]

    let builtinEnv: Environment =
        List.map (fun (x, y) -> (x, Builtin y)) builtinEnvList
        |> Map.ofList

    let addToEnv k v (env: Environment) = Map.add k v env

    let lookupEnv (env: Environment) symbol =
        match splitSelectArgument symbol with
        | Some (idx, isRight) ->
            if isRight then
                Builtin(selectR idx)
            else
                Builtin(selectL idx)
        | None ->
            match Map.tryFind symbol env with
            | Some x -> x
            | None -> failwithf $"Could not find {symbol} in environment."

module Interpreter =
    open Ast
    open Builtins

    let flip f x y = f y x

    let preserveBottom f =
        function
        | Bottom -> Bottom
        | x -> f x

    // Important note: All functions are bottom preserving.
    let rec interpretApplicationInner env func o =
        match func with
        | Function funcName ->
            match lookupEnv env funcName with
            | Builtin sym -> sym o
            | UserDefined sym -> interpretApplication env sym o
        | Composition (f, g) -> interpretApplication env f (interpretApplication env g o)

        | Constant (c) -> if o = Bottom then Bottom else c
        | FunctionList (funcs) ->
            let objects = List.map ((flip <| interpretApplication env) o) funcs

            if List.contains Bottom objects then
                Bottom
            else
                SequenceObject objects
        | ApplyToAll (func) ->
            match o with
            | SequenceObject xs ->
                let objects = List.map (interpretApplication env func) xs

                if List.contains Bottom objects then
                    Bottom
                else
                    SequenceObject objects
            | _ -> Bottom
        | BinaryToUnary (f, o') -> interpretApplication env f (SequenceObject([ o'; o ]))
        | Reduce f ->
            let reducer x y =
                interpretApplication env f (SequenceObject([ x; y ]))

            match o with
            | SequenceObject xs ->
                if List.isEmpty xs then
                    Bottom
                else
                    List.reduce reducer xs
            | _ -> Bottom
        | IfElse (c, f, g) ->
            match interpretApplication env c o with
            | x when x = (boolify true) -> interpretApplication env f o
            | x when x = (boolify false) -> interpretApplication env g o
            | _ -> failwith $"Condition {c} is not a predicate."

        | While (c, f) ->
            match interpretApplication env c o with
            | AtomObject (BoolAtom true) ->
                let object = interpretApplication env f o
                // recurse
                interpretApplication env func object
            | AtomObject (BoolAtom false) -> o
            | _ -> Bottom

    and interpretApplication env func o =
        preserveBottom (fun o -> interpretApplicationInner env func o) o

    let interpretExpr env expr =
        match expr with
        | Object _ -> expr
        | Application (def, o) -> Object(interpretApplication env def o)

    let interpretDef env (Definition (symbol, def)) = Map.add symbol (UserDefined def) env
