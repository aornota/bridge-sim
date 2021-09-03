module Aornota.BridgeSim.Domain.Json.Deal

open Aornota.BridgeSim.Domain.Deal

open Thoth.Json.Net

exception CannotDeserializeDealException of json:string * error:string

type Deal with
    static member FromJson json = match Decode.Auto.fromString<Deal> json with | Ok value -> value | Error error -> raise (CannotDeserializeDealException (json, error))
    member this.ToJson = Encode.Auto.toString<Deal> (4, this)
