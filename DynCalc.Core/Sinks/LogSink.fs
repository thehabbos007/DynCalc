namespace DynCalc.Core.Sinks

open System


module LogSink =
    open DynCalc.Lang.FP.Ast

    let rnGenerator = (Random())
    let nextId () = rnGenerator.Next()

    let logSink (_sinkContext: SinkContext) =
        { Connect = fun () -> async { () }
          Disconnect = fun () -> async { () }
          Publish = fun x -> async { printfn "%A" (objectToString x) } }
