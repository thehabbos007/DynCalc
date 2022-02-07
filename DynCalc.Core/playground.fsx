#r "nuget: FSharp.Control.Websockets"
#load "Providers/ProviderTypes.fs"
#load "Providers/WebSocketProvider.fs"
#load "ActorTypes.fs"
#load "ProviderActor.fs"
#load "SinkActor.fs"
#load "GlobalActor.fs"

open DynCalc.Core.Actors
open DynCalc.Core.Providers
open System.Net.WebSockets
open System
open System.Threading
open FSharp.Control.Websockets
open System.Collections.Concurrent

// Global actor
let ga = GlobalActor(ConcurrentDictionary())
ga.StartAction "test"

ga.Registry()
ga.Stop 531619977


// Provider example
let scheme = { WebSocketSchema.Url = "ws://127.0.0.1:8080" }

let provider = WebSocketProvider.webSocketProvider scheme

provider.Connect() |> Async.RunSynchronously
provider.Receive() |> Async.RunSynchronously
provider.Disconnect() |> Async.RunSynchronously


// Socket example
let socket = new ClientWebSocket()

async {
    do!
        socket.ConnectAsync(Uri("ws://ws.ifelse.io"), CancellationToken.None)
        |> Async.AwaitTask

    use clientWebSocket = ThreadSafeWebSocket.createFromWebSocket socket

    printfn "%A" clientWebSocket.State

    let! _ = ThreadSafeWebSocket.sendMessageAsUTF8 clientWebSocket "hello"
    let! _ = ThreadSafeWebSocket.receiveMessageAsUTF8 clientWebSocket
    let! response = ThreadSafeWebSocket.receiveMessageAsUTF8 clientWebSocket

    match response with
    | Ok data -> printfn $"{data}"
    | _ -> ()
}
|> Async.RunSynchronously
