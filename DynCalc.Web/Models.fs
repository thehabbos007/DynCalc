namespace DynCalc.Web.Models

[<CLIMutable>]
type Message = { Result: string; Data: string }

[<CLIMutable>]
type ProgramPayload = { Payload: string; Label: string }

[<CLIMutable>]
type RegistryResponse = { Id: int; Providers: int; Sinks: int }
