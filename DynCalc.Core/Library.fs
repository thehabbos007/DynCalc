module DynCalc.Core.Executor

open FSharp.Data.Adaptive

let adaptive () =
    let input = cval 0
    let output = AVal.map (fun x -> x * 1000) input

    (input, output)
