namespace DynCalc.Core.Providers

open System.Net.WebSockets
open System
open System.Threading
open FSharp.Data
open FSharp.Control.Websockets

module StockPriceProvider =
    open DynCalc.Lang.FP.Ast

    let connectWs schema =
        async {
            let socket = new ClientWebSocket()

            // TODO: URL for stock stuff is hard coded currently..
            do!
                socket.ConnectAsync(Uri("wss://ws.finnhub.io?token="), CancellationToken.None)
                |> Async.AwaitTask
            // Using the finnhub api https://finnhub.io/docs/api/websocket-trades
            // we can subscribe to ticker data
            // example for AAPL ticker: {"type":"subscribe","symbol":"AAPL"}

            do! WebSocket.sendMessageAsUTF8 socket (sprintf "{\"type\":\"subscribe\",\"symbol\":\"%s\"}" schema.Ticker)
            return ThreadSafeWebSocket.createFromWebSocket socket
        }

    let receiveWs webSocket =
        async {
            // Response is the form of
            // {"data":[{"c":["1"],"p":164.7,"s":"AAPL","t":1638300065742,"v":500}],"type":"trade"}
            // "p" is last price here, which we are interested in.
            // for simplicity we select the last data element
            let! response = ThreadSafeWebSocket.receiveMessageAsUTF8 webSocket

            let result =
                match response with
                | Ok (WebSocket.ReceiveUTF8Result.String payload) ->
                    let parsed = JsonValue.Parse payload

                    parsed.TryGetProperty("data")
                    |> Option.bind (function
                        | JsonValue.Array arr ->
                            Array.last arr
                            |> (fun x ->
                                x.TryGetProperty("p")
                                |> Option.map (fun x -> Ok(x.AsFloat())))
                        | _ -> None)
                    |> Option.defaultValue (Error(()))

                | _ -> Error(())

            return result

        }


    let disconenctWs webSocket =
        async {
            let! _result = ThreadSafeWebSocket.close webSocket WebSocketCloseStatus.NormalClosure "Closing"
            return ()
        }

    let ifState action =
        function
        | Some (state) ->
            async {
                let! result = action state
                return Some(result)
            }
        | _ -> async { return None }

    let connect scheme = connectWs scheme

    let disconnect state = ifState (disconenctWs) state

    let receive state = ifState (receiveWs) state

    let stockPriceProvider (schema: StockPriceSchema) _ =
        let mutable state = None

        { Connect =
            fun () ->
                async {
                    let! st = connect schema
                    state <- Some(st)
                }

          Disconnect = fun () -> async { do! disconnect state |> Async.Ignore }
          Receive =
            fun () ->
                async {
                    let! result = receive state

                    return
                        match result with
                        | (Some (Ok x)) -> numberfy x
                        | _ -> numberfy 0.0
                }

        }
