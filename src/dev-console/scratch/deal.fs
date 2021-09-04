module Aornota.BridgeSim.DevConsole.Scratch.Deal

open Aornota.BridgeSim.Common.Console
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Formatting.Deal
open Aornota.BridgeSim.Domain.Json.Deal
open Aornota.BridgeSim.Domain.Random.Deal

open System

let dealAndDiagrams () =
    writeNewLine "Testing Deal behaviour and diagram/s:\n\n" ConsoleColor.Magenta
    let deals = 2
    for n in 1..deals do
        let deal = Deal.MakeRandom()
        deal.Diagram(true) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.Cyan)
        writeBlankLine ()
        deal.Summary(n % 2 = 1, true) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.DarkCyan)
        if n < deals then writeNewLine "\t-----\n\n" ConsoleColor.DarkMagenta

let serialization () =
    writeNewLine "Testing Deal serialization and deserialization:\n" ConsoleColor.Magenta
    let deal = Deal.MakeRandom()
    writeNewLine "\tInitial deal:\n\n" ConsoleColor.Cyan
    deal.Summary(true, true) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.DarkCyan)
    let json = deal.ToJson
    writeNewLine "\tAfter round-trip serialization and deserialization:\n\n" ConsoleColor.Gray
    let deal = Deal.FromJson json
    deal.Summary(true, true) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.DarkGray)
