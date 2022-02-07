namespace DynCalc.Core.Providers

open System
open System.Net
open FSharp.Data

module HttpProvider =

    let receiveHttp httpSchema firstRun =
        async {
            if not firstRun then
                do! Async.Sleep(httpSchema.Interval)

            return! Http.AsyncRequestString(httpSchema.Url)
        }

    let receive state = receiveHttp state

    let HttpProvider (schema: HttpSchema) parserFunction =
        let mutable firstRun = true

        { Connect = fun () -> async { firstRun <- true }
          Disconnect = fun () -> async { () }
          Receive =
            fun () ->
                async {
                    let! res = receiveHttp schema firstRun
                    firstRun <- false

                    return parserFunction res
                } }
