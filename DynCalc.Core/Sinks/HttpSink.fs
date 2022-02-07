namespace DynCalc.Core.Sinks

open System


module HttpSink =
    open DynCalc.Lang.FP.Ast

    let httpSink (sinkContext: SinkContext) =
        { Connect = fun () -> async { () }
          Disconnect = fun () -> async { () }
          Publish =
            fun x ->
                async {
                    sinkContext.DataRegistry.AddOrUpdate(
                        sinkContext.Id,
                        (fun _ -> objectToString x),
                        (fun _ _ -> objectToString x)
                    )
                    |> ignore
                }

        }
