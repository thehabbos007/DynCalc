module DynCalc.Core.ActorTypes

open FSharp.Control

module ProviderTypes =
    type Payload = string

    type State =
        | Disconnected
        | Connected
        | Sending of Payload
        | Error

    type ErrorMessage = string

    type Message =
        | Connect
        | Disconnect
        | Send
        | Success
        | Failure of ErrorMessage
        | Resume


module SinkTypes =
    open DynCalc.Lang.FP.Ast

    type State =
        | Listening
        | Publishing
        | Error

    type ErrorMessage = string

    type Message =
        | Receive of ObjectGeneric<float>
        | Success
        | Failure of ErrorMessage
        | Reset
