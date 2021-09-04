module Aornota.BridgeSim.DevConsole.Scratch.Dds

open Aornota.BridgeSim.Common.Console
open Aornota.BridgeSim.Dds.Interop.Dds
open Aornota.BridgeSim.Domain.Auction
open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Evaluation.Core
open Aornota.BridgeSim.Domain.Formatting.Auction
open Aornota.BridgeSim.Domain.Formatting.Core
open Aornota.BridgeSim.Domain.Formatting.Deal
open Aornota.BridgeSim.Domain.Random.Deal

open System

let writeDoubleDummyResults (results:DoubleDummyResult list) =
    let forPosition (results:DoubleDummyResult array) =
        let level tricks = if tricks >= 7u then $"{tricks - 6u}" else "-"
        $"   {level results.[0].Tricks}   {level results.[1].Tricks}   {level results.[2].Tricks}   {level results.[3].Tricks}   {level results.[4].Tricks}"
    writeNewLine $"\t    {Club.Symbol}   {Diamond.Symbol}   {Heart.Symbol}   {Spade.Symbol}  {NoTrump.ShortText}" ConsoleColor.DarkGray
    write $"\n\t{North.ShortText}" ConsoleColor.DarkGray
    write (forPosition (results |> List.take 5 |> Array.ofList)) ConsoleColor.Gray
    write $"\n\t{South.ShortText}" ConsoleColor.DarkGray
    write (forPosition (results |> List.skip 10 |> List.take 5 |> Array.ofList)) ConsoleColor.Gray
    write $"\n\t{East.ShortText}" ConsoleColor.DarkGray
    write (forPosition (results |> List.skip 5 |> List.take 5 |> Array.ofList)) ConsoleColor.Gray
    write $"\n\t{West.ShortText}" ConsoleColor.DarkGray
    write (forPosition (results |> List.skip 15 |> List.take 5 |> Array.ofList)) ConsoleColor.Gray

let dds () =
    writeNewLine "Testing DDS interop:\n\n" ConsoleColor.Magenta
    let deal = Deal.MakeRandom()
    deal.Summary(true) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.Cyan)
    writeDoubleDummyResults (calculateDoubleDummy deal)
