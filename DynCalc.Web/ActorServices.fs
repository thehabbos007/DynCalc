module DynCalc.Web.ActorServices

open System.Collections.Concurrent

open DynCalc.Core.Actors

let dataMap = ConcurrentDictionary<int, string>()

let ga = GlobalActor(dataMap)

type IActorService =
    abstract GlobaActor: unit -> GlobalActor

type ActorService() =
    interface IActorService with
        member _.GlobaActor() = ga


type ISinkServiceMap =
    abstract DataMap: unit -> ConcurrentDictionary<int, string>

type SinkServiceMap() =
    interface ISinkServiceMap with
        member _.DataMap() = dataMap
