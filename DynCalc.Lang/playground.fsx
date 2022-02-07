#r "nuget: FParsec"
#r "nuget: FSharp.Data"
#r "nuget: FSharpx.Collections"
#load "DeriveParsers/JsonPath.fs"
#load "DeriveParsers/Derive.fs"
#load "FpSys.fs"
#load "Ast.fs"
#load "Parser.fs"
#load "MultilineParser.fs"

open FParsec
open DynCalc.Lang.Ast
// open DynCalc.Lang.Inference
open DynCalc.Lang.Parser
open DynCalc.Lang.MultilineParser


// let infer (xs) =
//     try
//         let t = typeInference Map.empty xs
//         printfn "%A " t
//     with
//     | ex -> printfn "ERROR %O" ex



let xs =
    unwrapParseProgram
        """
type StockPrice :: Number[] deriving (json "$.data[*].p")
btc_price <- ws("wss://ws.finnhub.io?token=","{\"type\":\"subscribe\",\"symbol\":\"BINANCE:BTCUSDT\"}") as StockPrice

func avg = /.[%+,length]
func mult_6 = *.[#6.6,id]
func avg_in_dkk = mult_6.avg

btc_avg_price_in_dkk = avg_in_dkk:btc_price;
btc_avg_price_in_dkk -> log();
"""


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


#r "nuget: FSharp.Data"
open FSharp.Data

let str =
    "{\"data\":[{\"c\":[\"1\"],\"p\":164.7,\"s\":\"AAPL\",\"t\":1638300065742,\"v\":500}],\"type\":\"trade\"}"

let parsed = JsonValue.Parse str

parsed.TryGetProperty("data")
|> Option.bind (function
    | JsonValue.Array arr -> Array.last arr |> (fun x -> x.TryGetProperty("p"))
    | _ -> None)


#r "nuget: FParsec"
#r "nuget: FSharpx.Collections"
#load "FpSys.fs"

open FParsec
open FSharpx.Collections
open DynCalc.Lang.FP.Ast
open DynCalc.Lang.FP.Parser
open DynCalc.Lang.FP.Builtins
open DynCalc.Lang.FP.Interpreter

type TestProgram = TestProgram of Definition list * Expression

let testProgram =
    wsIgnore (many definition) .>>. expression
    |>> TestProgram

let interpret (TestProgram (defs, expr)) =
    let newEnv = List.fold (interpretDef) builtinEnv defs
    interpretExpr newEnv expr


run
    (testProgram)
    """Def fac = if eq0 -> _1;*.[id, fac.sub1]
Def eq0 = eq.[id,_0]
Def sub1 = -.[id,_1]
fac:5"""

run (definition) "Def fac = if eq0 -> _1;(*.[id, fac.sub1])"

run (testProgram) "@length:<<A>, <A, B>, <A, B, C>>"
run (testProgram) "/:<9,3>"

let (TestProgram (_, res)) =
    match (run (testProgram) "distl:<A, <1,2,3,4>>") with
    | Success (x, _, _) -> x
    | Failure (x, _, _) -> failwithf "Failed to parse, %s" x


let test1 =
    match
        run
            (testProgram)
            """func avg = / . [%+, length]
func avg_in_dkk = * . [id . 0, avg . 1]
avg_in_dkk:<6.6,<1,2,3>>"""
        with
    | Success (x, _, _) -> x
    | x -> failwith $"something went wrong with parsing. {x}"

interpret test1


let test4 =
    match run (testProgram) """(bu eq 1.5).and:1.3""" with
    | Success (x, _, _) -> x
    | x -> failwith "something went wrong with parsing. {x}"

interpret test4

let rec transpose lst =
    match lst with
    | (_ :: _) :: _ ->
        List.map List.head lst
        :: transpose (List.map List.tail lst)
    | _ -> []

FSharpx.Collections.List.transpose [ [ 1; 2; 3 ]
                                     [ 4; 5; 6; 7 ]
                                     [] ]

let test =
    match
        run
            (testProgram)
            """Def fac = if eq0 -> _1;*.[id, fac.sub1]
    Def eq0 = eq.[id,_0]
    Def sub1 = -.[id,_1]
    fac:5"""
        with
    | Success (x, _, _) -> x
    | x -> failwith $"something went wrong with parsing. {x}"

interpret test

let test3 =
    match
        run
            (testProgram)
            """
func avg = /.[%+,length]
func mult6 = *.[#6.6,id]
func avg_in_dkk = mult6.avg
mult6:2.3
"""
    // avg:<47385.39,47385.38,47382.17,47381.88,47381.82,47382.17,47382.17,47382.18,47382.18,47382.18>"""
        with
    | Success (x, _, _) -> x
    | x -> failwith $"something went wrong with parsing. {x}"

interpret test3


// [AtomObject (NumberAtom 47385.39,); AtomObject (NumberAtom 47385.38,);
//    AtomObject (NumberAtom 47382.17,); AtomObject (NumberAtom 47381.88,);
//    AtomObject (NumberAtom 47381.82,); AtomObject (NumberAtom 47382.17,);
//    AtomObject (NumberAtom 47382.17,); AtomObject (NumberAtom 47382.18,);
//    AtomObject (NumberAtom 47382.18,); AtomObject (NumberAtom 47382.18,)]


distL (
    SequenceObject [ AtomObject(SymbolAtom "A")
                     SequenceObject [ AtomObject(NumberAtom 1.0)
                                      AtomObject(NumberAtom 2.0)
                                      AtomObject(NumberAtom 3.0)
                                      AtomObject(NumberAtom 4.0) ] ]
)
