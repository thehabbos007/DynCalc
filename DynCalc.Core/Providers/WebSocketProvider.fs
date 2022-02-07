namespace DynCalc.Core.Providers

open System.Net.WebSockets
open System
open System.Threading
open FSharp.Control.Websockets

module WebSocketProvider =
    let connectWs (schema: WebSocketSchema) =
        async {
            let socket = new ClientWebSocket()

            do!
                socket.ConnectAsync(Uri(schema.Url), CancellationToken.None)
                |> Async.AwaitTask

            if schema.FirstMessage.IsSome then
                do! WebSocket.sendMessageAsUTF8 socket schema.FirstMessage.Value


            return ThreadSafeWebSocket.createFromWebSocket socket
        }

    let receiveWs webSocket =
        async {
            let! response = ThreadSafeWebSocket.receiveMessageAsUTF8 webSocket

            return
                match response with
                | Ok (WebSocket.ReceiveUTF8Result.String text) -> Ok(text)
                | _ -> Error(())

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

    let connect schema = connectWs schema

    let disconnect state = ifState (disconenctWs) state

    let receive state = ifState (receiveWs) state

    let webSocketProvider (schema: WebSocketSchema) parserFunction =
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
                        | (Some (Ok x)) -> parserFunction x
                        | _ -> parserFunction "0.0"
                }

        }
