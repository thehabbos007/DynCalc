namespace DynCalc.Core.Actors

open System
open System.Threading
open FSharp.Data.Adaptive
open DynCalc.Lang.FP.Ast
open DynCalc.Core.ActorTypes.SinkTypes
open DynCalc.Core.Sinks


module SinkControl =
    let constructSink (schema: SinkSchemas) (sinkContext: SinkContext) =
        match schema with
        | Http -> HttpSink.httpSink sinkContext
        | Log -> LogSink.logSink sinkContext


type SinkActor
    (
        signalStopping,
        label: string,
        sinkContext: SinkContext,
        schema: SinkSchemas,
        adaptive: aval<ObjectGeneric<float>>
    ) =
    let cts = new CancellationTokenSource()


    let agent =
        MailboxProcessor<Message>.Start
            ((fun inbox ->
                printfn $"SinkActor spawned: {label}"

                let sink = SinkControl.constructSink schema sinkContext

                async { do! sink.Connect() }
                |> Async.StartImmediate


                let rec listeningState () =
                    async {

                        let! msg = inbox.Receive()

                        match msg with
                        | Receive x -> return! publishingState x
                        | _ -> return! listeningState ()
                    }

                and publishingState data =
                    async {
                        do! sink.Publish data
                        return! listeningState ()
                    }

                and errorState error =
                    async {
                        do! sink.Disconnect()
                        do! sink.Connect()
                        return! listeningState ()
                    }


                listeningState ()),
             cancellationToken = cts.Token)

    let disposable = adaptive.AddCallback(fun o -> agent.Post(Receive o))

    let _ =
        agent.Error.Add (fun e ->
            printfn "%A" e
            signalStopping ())

    member _.Mailbox = agent
    member _.Post = agent.Post
    member _.PostData data = agent.Post(Receive data)

    member _.Cancel() =
        disposable.Dispose()
        cts.Cancel()
