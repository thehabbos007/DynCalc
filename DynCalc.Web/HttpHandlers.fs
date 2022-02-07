module DynCalc.Web.HttpHandlers

open System.Collections.Generic
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks
open Giraffe
open DynCalc.Web.Models
open DynCalc.Core.Actors
open DynCalc.Web.ActorServices
open DynCalc.Lang.MultilineParser

let handleRegistry =
    fun (next: HttpFunc) (ctx: HttpContext) ->
        let ga = ctx.GetService<IActorService>().GlobaActor()

        let registry =
            ga.Registry().ToArray()
            |> Array.map (fun (pair: KeyValuePair<CompositeProgramIdentifier, _>) ->
                let (_, provs, sinks) = pair.Value

                { Id = pair.Key
                  Providers = provs.Length
                  Sinks = sinks.Length })

        task { return! json registry next ctx }


let handleSinkData =
    fun (id: int) (next: HttpFunc) (ctx: HttpContext) ->
        let mapData = ctx.GetService<ISinkServiceMap>().DataMap()

        let (found, value) = mapData.TryGetValue(id)


        task {
            if found then
                let result = { Result = "Success"; Data = value }

                return! json result next ctx
            else
                let result = { Result = "Error"; Data = "" }
                return! json result next ctx
        }

let handleUndeploy =
    fun (id: int) (next: HttpFunc) (ctx: HttpContext) ->
        let ga = ctx.GetService<IActorService>().GlobaActor()

        task {
            match (ga.Stop id) with
            | Some (Handled (x)) ->
                let response =
                    { Result = "Success"
                      Data = sprintf "%A" x }

                return! json response next ctx
            | Some (Error (x)) ->
                let response = { Result = "Error"; Data = x }
                return! json response next ctx
            | _ ->
                let response = { Result = "Error"; Data = "" }
                return! json response next ctx
        }

let handleNewProgram =
    fun (next: HttpFunc) (ctx: HttpContext) ->
        let ga = ctx.GetService<IActorService>().GlobaActor()

        task {
            let! payload = ctx.BindJsonAsync<ProgramPayload>()

            match (ga.StartProgram payload.Label payload.Payload) with
            | Some (Handled (x)) ->
                let response =
                    { Result = "Success"
                      Data = sprintf "%A" x }

                return! json response next ctx
            | Some (Error (x)) ->
                let response = { Result = "Error"; Data = x }
                return! json response next ctx
            | _ ->
                let response = { Result = "Error"; Data = "" }
                return! json response next ctx
        }

let handleProgramAst =
    fun (next: HttpFunc) (ctx: HttpContext) ->
        task {
            let! payload = ctx.BindJsonAsync<ProgramPayload>()

            match parseResult payload.Payload with
            | Good x ->
                let response =
                    { Result = "Success"
                      Data = sprintf "%A" x }

                return! json response next ctx
            | Bad x ->
                let response = { Result = "Error"; Data = x }
                return! json response next ctx
        }
