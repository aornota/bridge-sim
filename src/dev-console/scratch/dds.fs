module Aornota.BridgeSim.DevConsole.Scratch.Dds

open Aornota.BridgeSim.Common.Console
open Aornota.BridgeSim.Dds.Interop.Dds
open Aornota.BridgeSim.DevConsole.Common
open Aornota.BridgeSim.Domain.Auction
open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Formatting.Auction
open Aornota.BridgeSim.Domain.Formatting.Core
open Aornota.BridgeSim.Domain.Formatting.Deal
open Aornota.BridgeSim.Domain.Random.Deal

open System

let writeDoubleDummyResults (results:DoubleDummyResults) =
    let forPosition (position) =
        let level strain = match results.Level(position, strain) with | Some level -> level.ShortText | None -> "-"
        $"   {level (Suit Club)}   {level (Suit Diamond)}   {level (Suit Heart)}   {level (Suit Spade)}   {level NoTrump}"
    writeNewLine $"\t    {Club.Symbol}   {Diamond.Symbol}   {Heart.Symbol}   {Spade.Symbol}  {NoTrump.ShortText}" ConsoleColor.DarkGray
    write $"\n\t{North.ShortText}" ConsoleColor.DarkGray
    write (forPosition North) ConsoleColor.Gray
    write $"\n\t{South.ShortText}" ConsoleColor.DarkGray
    write (forPosition South) ConsoleColor.Gray
    write $"\n\t{East.ShortText}" ConsoleColor.DarkGray
    write (forPosition East) ConsoleColor.Gray
    write $"\n\t{West.ShortText}" ConsoleColor.DarkGray
    write (forPosition West) ConsoleColor.Gray

let dds count interactive =
    if count = 0 then raise CountMustBeGreaterThanZeroException
    writeNewLine "Testing DDS interop:\n\n" ConsoleColor.Magenta
    for n in 1..count do
        let deal = Deal.MakeRandom()
        deal.Diagram(true) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.Cyan)
        writeDoubleDummyResults (calculateDoubleDummy deal)
        if n < count then
            if interactive then
                writeNewLine "\nPress any key to continue...\n" ConsoleColor.DarkMagenta
                Console.ReadKey () |> ignore
                writeBlankLine ()
            else writeNewLine "\n\t-----\n\n" ConsoleColor.DarkMagenta
