module Aornota.BridgeSim.DevConsole.Scratch.Dds

open Aornota.BridgeSim.Common.Console
open Aornota.BridgeSim.Dds.Interop.Dds
open Aornota.BridgeSim.DevConsole.Common
open Aornota.BridgeSim.Domain.Auction
open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Evaluation.Core
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

let forcingOpening2C count interactive =
    if count = 0 then raise CountMustBeGreaterThanZeroException
    writeNewLine "DDS for 2C forcing opening hands:\n\n" ConsoleColor.Magenta
    let generate _ =
        let rec check (deal:Deal) positions =
            match positions with
            | [] -> None
            | position :: remaining ->
                let hand = deal.Hand(position)
                match hand.ShapeCategory = Balanced, hand.Hcp, hand.Ltc with
                | true, hcp, _ when hcp >= 22<hcp> -> Some deal // balanced with 22+ HCP
                | false, _, ltc when ltc <= 4 -> Some deal // not balanced with 4- LTC
                | _ -> check deal remaining
        check (Deal.MakeRandom()) [ North ; East; South ; West ]
    let generator = Seq.initInfinite generate |> Seq.choose id
    generator |> Seq.take count |> Seq.iteri (fun i deal ->
        deal.Diagram(true) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.Cyan)
        writeBlankLine ()
        deal.Summary(false, false) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.DarkCyan)
        writeDoubleDummyResults (calculateDoubleDummy deal)
        if i + 1 < count then
            if interactive then
                writeNewLine "\nPress any key to continue...\n" ConsoleColor.DarkMagenta
                Console.ReadKey () |> ignore
                writeBlankLine ()
            else writeNewLine "\n\t-----\n\n" ConsoleColor.DarkMagenta)

let forcingOpening1C count interactive =
    if count = 0 then raise CountMustBeGreaterThanZeroException
    writeNewLine "DDS for forcing 1C opening hands:\n\n" ConsoleColor.Magenta
    let generate _ =
        let rec check (deal:Deal) positions =
            match positions with
            | [] -> None
            | position :: remaining ->
                let hand = deal.Hand(position)
                if hand.Hcp >= 16<hcp> then Some deal // 16+ HCP (any shape)
                else check deal remaining
        check (Deal.MakeRandom()) [ North ; East; South ; West ]
    let generator = Seq.initInfinite generate |> Seq.choose id
    generator |> Seq.take count |> Seq.iteri (fun i deal ->
        deal.Diagram(true) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.Cyan)
        writeBlankLine ()
        deal.Summary(false, false) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.DarkCyan)
        writeDoubleDummyResults (calculateDoubleDummy deal)
        if i + 1 < count then
            if interactive then
                writeNewLine "\nPress any key to continue...\n" ConsoleColor.DarkMagenta
                Console.ReadKey () |> ignore
                writeBlankLine ()
            else writeNewLine "\n\t-----\n\n" ConsoleColor.DarkMagenta)

let forcingOpening1CAnd1HResponse count interactive =
    if count = 0 then raise CountMustBeGreaterThanZeroException
    writeNewLine "DDS for forcing 1C opening hands and 1H response:\n\n" ConsoleColor.Magenta
    let generate _ =
        let rec check (deal:Deal) =
            if deal.Hand(North).Hcp >= 16<hcp> then
                let south = deal.Hand(South)
                if south.Hcp >= 9<hcp> && south.ShapeCategory = Balanced then
                    Some deal
                else None
            else None
        check (Deal.MakeRandom())
    let generator = Seq.initInfinite generate |> Seq.choose id
    generator |> Seq.take count |> Seq.iteri (fun i deal ->
        deal.Diagram(true) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.Cyan)
        writeBlankLine ()
        deal.Summary(false, false) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.DarkCyan)
        writeDoubleDummyResults (calculateDoubleDummy deal)
        if i + 1 < count then
            if interactive then
                writeNewLine "\nPress any key to continue...\n" ConsoleColor.DarkMagenta
                Console.ReadKey () |> ignore
                writeBlankLine ()
            else writeNewLine "\n\t-----\n\n" ConsoleColor.DarkMagenta)

let forcingOpening1CAnd1SResponse count interactive =
    if count = 0 then raise CountMustBeGreaterThanZeroException
    writeNewLine "DDS for forcing 1C opening hands and 1S response:\n\n" ConsoleColor.Magenta
    let generate _ =
        let rec check (deal:Deal) =
            if deal.Hand(North).Hcp >= 16<hcp> then
                let south = deal.Hand(South)
                if south.Hcp >= 9<hcp> && (south.Shape = FiveFourFourZero || south.Shape = FourFourFourOne) then
                    Some deal
                else None
            else None
        check (Deal.MakeRandom())
    let generator = Seq.initInfinite generate |> Seq.choose id
    generator |> Seq.take count |> Seq.iteri (fun i deal ->
        deal.Diagram(true) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.Cyan)
        writeBlankLine ()
        deal.Summary(false, false) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.DarkCyan)
        writeDoubleDummyResults (calculateDoubleDummy deal)
        if i + 1 < count then
            if interactive then
                writeNewLine "\nPress any key to continue...\n" ConsoleColor.DarkMagenta
                Console.ReadKey () |> ignore
                writeBlankLine ()
            else writeNewLine "\n\t-----\n\n" ConsoleColor.DarkMagenta)
