namespace DynCalc.Core.Sinks

open System.Collections.Concurrent

type SinkContract<'a> =
    { Connect: unit -> Async<unit>
      Disconnect: unit -> Async<unit>
      Publish: 'a -> Async<unit> }


type SinkContext =
    { Id: int
      DataRegistry: ConcurrentDictionary<int, string> }

type SinkSchemas =
    | Http
    | Log


module Mapper =
    open FSharp.Data.Adaptive
    open DynCalc.Lang.Ast
    open DynCalc.Lang.FP.Ast

    let mapSinkSchema functionExpr =
        match functionExpr with
        | FunctionCall ("http", []) -> Http
        | FunctionCall ("log", []) -> Log
        | FunctionCall (f, p) -> failwithf $"provider {f}{p} not implemented"
