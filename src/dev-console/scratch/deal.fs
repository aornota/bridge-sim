module Aornota.BridgeSim.DevConsole.Scratch.Deal

open Aornota.BridgeSim.Common.Console
open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Formatting.Deal

open System

let private random = Random()

let dealAndDiagrams () =
    writeNewLine "Testing Deal behaviour and diagram/s:\n\n" ConsoleColor.Magenta
    let randopmDealer () = match random.Next(4) with | 0 -> North | 1 -> East | 2 -> South | _ -> West
    let randomVulnerability () = match random.Next(2) with | 0 -> NotVulnerable | _ -> Vulnerable
    let deals = 2
    for n in 1..deals do
        let deal = Deal.Make(randopmDealer (), randomVulnerability (), randomVulnerability ())
        deal.Diagram(true) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.Cyan)
        //deal.Summary(true) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.DarkCyan)
        if n < deals then writeNewLine "\t-----\n\n" ConsoleColor.DarkMagenta
