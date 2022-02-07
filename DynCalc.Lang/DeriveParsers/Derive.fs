namespace DynCalc.Lang.Derive

open FSharp.Data

module Derive =
    type DeriveParser = Json of string

    let deriveNumber parser data =
        match parser with
        | Json x ->
            let value = JsonValue.Parse data |> JsonPath.find x
            value.AsFloat()


    let deriveNumberList parser data =
        match parser with
        | Json x ->
            JsonValue.Parse data
            |> JsonPath.findList x
            |> List.map (fun x -> x.AsFloat())
