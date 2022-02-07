namespace DynCalc.Core.Actors

open System
open System.Threading
open FSharp.Data.Adaptive
open DynCalc.Core.Executor
open DynCalc.Core.Runtime
open DynCalc.Core.Actors
open DynCalc.Lang.Parser
open DynCalc.Lang.FP.Ast


type ProgramActorMessage = | Stop

type ProgramActor(signalStopping, program: Context<string, ObjectGeneric<float>>) =

    let cts = new CancellationTokenSource()

    let agent =
        MailboxProcessor<ProgramActorMessage>.Start
            ((fun inbox ->
                let _program = program
                printfn "Starting program"

                let mutable earlyReturn = false

                let rec messageLoop () =
                    async {
                        // read a message
                        let! msg = inbox.Receive()

                        match msg with
                        | Stop -> earlyReturn <- true

                        if earlyReturn then
                            return ()
                        else
                            return! messageLoop ()
                    }

                messageLoop ()),
             cancellationToken = cts.Token)

    let _ =
        agent.Error.Add (fun e ->
            printfn "%A" e
            signalStopping ())

    member _.Mailbox = agent
    member _.Post = agent.Post

    member _.Cancel() = cts.Cancel()
