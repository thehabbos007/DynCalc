#r "nuget: FSharp.Data"
#r "nuget: FSharp.Data.Adaptive"
#r "nuget: FParsec"
#r "nuget: FSharpPlus"
#load "../../DynCalc.Lang/DeriveParsers/JsonPath.fs"
#load "../../DynCalc.Lang/DeriveParsers/Derive.fs"
#load "../../DynCalc.Lang/FpSys.fs"
#load "../../DynCalc.Lang/Ast.fs"
#load "../../DynCalc.Lang/Parser.fs"
#load "../../DynCalc.Lang/MultilineParser.fs"
#load "../Providers/ProviderTypes.fs"
#load "../Sinks/SinkTypes.fs"
#load "Graph.fs"
#load "Evaluator.fs"

open FParsec
open FSharp.Data.Adaptive
open DynCalc.Lang.FP.Parser
open DynCalc.Lang.FP.Builtins
open DynCalc.Lang.MultilineParser
// open DynCalc.Core.Runtime.Graph
// open DynCalc.Core.Runtime
open DynCalc.Core.Runtime.Evaluator
open DynCalc.Lang.FP.Ast

unwrapParseProgram "Def a = [_6.6,_2];"

unwrapParseProgram
    "type StockPrice :: Number[] deriving (json \"$.data[*].p\"); btc_price <- ws(\"wss://ws.finnhub.io?token=\") as StockPrice;  func avg = /.[%+,length]; func mult_6 = *.[#6.6,id]; func avg_in_dkk = mult_6.avg;  btc_avg_price_in_dkk = avg_in_dkk:btc_price;"
|> evaluateG
// let refs = referencesFromAst programAst
// let dps = dependants refs programAst
// graphFromProgram programAst |> toposort

(*
{
	"label": "program",
	"payload": "A1 = 2*100*ws(\"ws://127.0.0.1:8080\");A2 = A4*22;A3 = A2 *10 * A1;A4 = A1+1;"
}
*)


let avalify = AVal.map (id)

let c1 = cval 1
let c2 = cval 2

type Object<'a> =
    | Ref of 'a
    | Const of int
    | SequenceObject of Object<'a> list

let output =
    SequenceObject [ Ref(avalify c1)
                     SequenceObject [ Ref(avalify c1)
                                      Ref(avalify c2)
                                      Const 3 ]
                     Const 3 ]

let rec flatten (o: Object<aval<int>>) =
    match o with
    | Ref x -> AVal.map (Ref) x
    | Const c -> AVal.constant (Const c)
    | SequenceObject os ->
        (AList.mapA (flatten) (AList.ofList os)).Content
        |> AVal.map (IndexList.toList >> SequenceObject)

let nest = flatten output

AVal.force nest
transact (fun () -> c1.Value <- 2)


let list = AVal.bind (fun a1 -> AVal.map (fun a2 -> [ a1; a2; 3 ]) c2) c1

AVal.force list
transact (fun () -> c1.Value <- 2)



let c1' = cval (numberfy 1.0)
let c2' = cval (numberfy 4.0)

let ctx =
    { ctxWithGraph [||] with
        Intermediates =
            Map.ofList [ ("a1", c1' :> aval<_>)
                         ("a2", c2' :> aval<_>) ] }

let test1 =
    match run (expression) """(@+).trans:<<a1,2,3>,<a2,5,6>>""" with
    | Success (x, _, _) -> x
    | x -> failwith $"something went wrong with parsing. {x}"

let (_, res) = interpretExpr ctx "a3" test1
let av = res.Value
AVal.force res.Value
transact (fun () -> c1'.Value <- numberfy 8.0)

av.AddCallback(fun x -> printfn $"CALBACK RES: {x}")
