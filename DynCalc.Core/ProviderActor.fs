namespace DynCalc.Core.Actors

open System
open System.Threading
open FSharp.Data.Adaptive
open DynCalc.Lang.Ast
open DynCalc.Lang.FP.Ast
open DynCalc.Core.ActorTypes.ProviderTypes
open DynCalc.Core.Actors
open DynCalc.Core.Providers
open DynCalc.Lang.Derive.Derive

module ProviderControl =
    let constructProvider (schema: ProviderSchemas) parserFunction =
        match schema with
        | WebSocket schema -> WebSocketProvider.webSocketProvider schema parserFunction
        | StockPrice schema -> StockPriceProvider.stockPriceProvider schema parserFunction
        | Http schema -> HttpProvider.HttpProvider schema parserFunction

type ProviderActor
    (
        signalStopping,
        label: string,
        changable: cval<ObjectGeneric<float>>,
        parser: ParserType,
        schema: ProviderSchemas
    ) =
    let doConnect provider =
        printfn "doConnect"
        provider.Connect()


    let doDisconnect provider =
        printfn "doDisconnect"
        provider.Disconnect()

    let doSend provider =
        async {
            let! res = provider.Receive()
            //printfn $"Got data: {res} from provider"
            transact (fun () -> changable.Value <- res)
        }

    let doError error = printfn $"doError, {error}"

    let cts = new CancellationTokenSource()

    let agent =
        let (typ, parserType) = parser

        let parserFunction: (string -> ObjectGeneric<float>) =
            match parser with
            | AstNumber, None -> float >> numberfy
            | AstNumber, Some (parser) -> deriveNumber parser >> numberfy
            | AstNumberList, Some (parser) ->
                deriveNumberList parser
                >> (List.map (numberfy))
                >> SequenceObject
            | _ -> failwith $"Combination of {typ} and deriver {parserType} not implemented."


        MailboxProcessor<Message>.Start
            ((fun inbox ->
                printfn $"ProviderActor spawned: {label}"

                let provider = ProviderControl.constructProvider schema parserFunction

                let rec disconnectedState () =
                    async {
                        do! doDisconnect provider

                        let! msg = inbox.Receive()

                        match msg with
                        | Connect -> return! connectedState ()
                        | _ -> return! disconnectedState ()
                    }

                and connectedState () =
                    async {
                        do! doConnect provider
                        return! pollingState ()

                    }

                and pollingState () =
                    async {
                        do! doSend provider
                        return! pollingState ()
                    }

                and errorState error =
                    async {
                        doError error
                        return! connectedState ()
                    }


                connectedState ()),
             cancellationToken = cts.Token)

    let _ =
        agent.Error.Add (fun e ->
            printfn "%A" e
            signalStopping ())

    member _.Mailbox = agent
    member _.Post = agent.Post

    member _.Send() =
        agent.Post(Connect)
        agent.Post(Send)

    member _.Cancel() = cts.Cancel()
