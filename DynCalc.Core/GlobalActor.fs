namespace DynCalc.Core.Actors

open System
open FSharp.Data.Adaptive
open System.Threading
open DynCalc.Core.Actors
open DynCalc.Core.Providers
open DynCalc.Core.Sinks
open FSharp.Data.Adaptive
open DynCalc.Core.Runtime
open DynCalc.Core.Runtime.Evaluator
open System.Collections.Concurrent
open DynCalc.Lang.MultilineParser


type CompositeProgramIdentifier = int

type GlobalActorReply =
    | Handled of int
    | Error of string

type GlobalActorMessage =
    | SetupSchema of string * string * AsyncReplyChannel<GlobalActorReply> option
    | StopActor of CompositeProgramIdentifier * AsyncReplyChannel<GlobalActorReply> option
    | ChildStopped of CompositeProgramIdentifier


type GlobalActor(dataRegistry: ConcurrentDictionary<int, string>) =

    let cts = new CancellationTokenSource()
    let rnGenerator = (Random())
    let nextId () = rnGenerator.Next()


    let mutable registry =
        ConcurrentDictionary<CompositeProgramIdentifier, ProgramActor * ProviderActor list * SinkActor list>()


    let agent =
        MailboxProcessor<GlobalActorMessage>.Start
            ((fun inbox ->
                printfn "Starting Global"

                let signalStopping id () = inbox.Post(ChildStopped id)

                let rec mainState () =
                    async {
                        let! msg = inbox.Receive()


                        match msg with
                        | SetupSchema (_label, program, replyOpt) ->
                            let id = nextId ()

                            printfn $"Got id {id}"

                            let parsed =
                                match parseResult program with
                                | Good x ->
                                    printfn $"Got parsed {x}"
                                    Some(evaluateG x)
                                | Bad x ->
                                    replyOpt.Value.Reply(Error x)
                                    None

                            if parsed.IsNone then
                                if replyOpt.IsSome then
                                    replyOpt.Value.Reply(Handled 0)
                                    return! mainState ()
                                else
                                    return! mainState ()

                            let parsed = parsed.Value

                            let program = ProgramActor(signalStopping id, parsed)

                            let providers =
                                List.mapi
                                    (fun i (label, (schema, value, typeParser)) ->
                                        ProviderActor(signalStopping id, $"{label}-p-{i}", value, typeParser, schema))
                                    (Map.toList parsed.Inputs)

                            let sinkContext = { Id = id; DataRegistry = dataRegistry }

                            let sinks =
                                List.mapi
                                    (fun i (label, schema, value) ->
                                        SinkActor(signalStopping id, $"{label}-s-{i}", sinkContext, schema, value))
                                    parsed.Outputs

                            registry.TryAdd(id, (program, providers, sinks))
                            |> ignore

                            if replyOpt.IsSome then
                                replyOpt.Value.Reply(Handled id)
                            else
                                ()

                        | StopActor (id, replyOpt) ->
                            let (success, (p, providers, sinks)) = registry.TryRemove(id)

                            if success then
                                printfn $"Stopping {id} - Stopping Program actor"
                                p.Cancel() // ProgramActor

                                printfn $"Stopping {List.length providers} providers"
                                List.iter (fun (x: ProviderActor) -> x.Cancel()) providers

                                printfn $"Stopping {List.length sinks} sinks"
                                List.iter (fun (x: SinkActor) -> x.Cancel()) sinks
                                let _ = dataRegistry.TryRemove(id)

                                if replyOpt.IsSome then
                                    replyOpt.Value.Reply(Handled id)

                                ()
                            else
                                if replyOpt.IsSome then
                                    replyOpt.Value.Reply(Error "Could not stop deployment.")

                                ()
                        | ChildStopped id ->
                            let (success, (p, providers, sinks)) = registry.TryRemove(id)

                            if success then
                                printfn $"Stopping {id} - Stopping Program actor"
                                p.Cancel() // ProgramActor

                                printfn $"Stopping {id} - Stopping {List.length providers} providers"
                                List.iter (fun (x: ProviderActor) -> x.Cancel()) providers

                                printfn $"Stopping {id} - Stopping {List.length sinks} sinks"
                                List.iter (fun (x: SinkActor) -> x.Cancel()) sinks
                                let _ = dataRegistry.TryRemove(id)

                                ()


                        return! mainState ()
                    }



                mainState ()),
             cancellationToken = cts.Token)

    let _ = agent.Error.Add(fun e -> printfn "%A" e)

    member _.StartProgram label payload =
        agent.TryPostAndReply((fun reply -> SetupSchema(label, payload, Some reply)), 5000)

    member _.Stop id =
        agent.TryPostAndReply((fun reply -> StopActor(id, Some reply)), 5000)

    member _.Registry() = registry


    member _.Shutdown() = cts.Cancel()
