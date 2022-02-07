namespace DynCalc.Core.Providers

type ProviderContract<'a> =
    { Connect: unit -> Async<unit>
      Disconnect: unit -> Async<unit>
      Receive: unit -> Async<'a> }


type WebSocketSchema =
    { Url: string
      FirstMessage: string option }

type StockPriceSchema = { Ticker: string }

type HttpSchema = { Url: string; Interval: int }

type ProviderSchemas =
    | WebSocket of WebSocketSchema
    | StockPrice of StockPriceSchema
    | Http of HttpSchema


module Mapper =
    open FSharp.Data.Adaptive
    open DynCalc.Lang.Ast
    open DynCalc.Lang.FP.Ast

    let mapProviderSchema functionExpr =
        match functionExpr with
        | FunctionCall ("ws", [ url; message ]) ->
            let wsSchema =
                WebSocket
                    { Url = url
                      FirstMessage = Some(message) }

            let changable = cval (Bottom)

            (wsSchema, changable)
        | FunctionCall ("ws", [ url ]) ->
            let wsSchema = WebSocket { Url = url; FirstMessage = None }
            let changable = cval (Bottom)

            (wsSchema, changable)

        | FunctionCall ("ticker", [ ticker ]) ->
            let wsSchema = StockPrice { Ticker = ticker }
            let changable = cval (Bottom)
            (wsSchema, changable)

        | FunctionCall ("http", [ url; interval ]) ->
            let wsSchema = Http { Url = url; Interval = int interval }
            let changable = cval (Bottom)
            (wsSchema, changable)

        | FunctionCall (f, p) -> failwithf $"provider {f}{p} not implemented"
