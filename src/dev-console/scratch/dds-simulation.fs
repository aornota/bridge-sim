module Aornota.BridgeSim.DevConsole.Scratch.DdsSimulation

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

open System

let private openerPosition = North // arbitrary
let private vulnerability = NotVulnerable // arbitrary

let private exampleAuction = "1C-1S-1NT-2H-2S-3H-6S (declared by responder)"

let private opener =
    Hand.Make [
        Card.Make (Ace, Spade)
        Card.Make (Queen, Spade)
        Card.Make (Seven, Spade)
        Card.Make (Four, Spade)
        Card.Make (King, Heart)
        Card.Make (Eight, Heart)
        Card.Make (Five, Heart)
        Card.Make (Three, Heart)
        Card.Make (Two, Heart)
        Card.Make (Ace, Diamond)
        Card.Make (Jack, Diamond)
        Card.Make (King, Club)
        Card.Make (Queen, Club)
    ]

(* let private responderMatches (hand:Hand) =
    let shapeMatches =
        match hand.SuitCounts with
        | 4, 1, 4, 4
        | 5, 0, 4, 4
        | 4, 0, 5, 4
        | 4, 0, 4, 5 -> true
        | _ -> false
    shapeMatches && hand.Hcp >= 9<hcp> && hand.ControlCount = 4 *)
(* TEMP-NMB: Only with void in hearts... *)
let private responderMatches (hand:Hand) =
    let shapeMatches =
        match hand.SuitCounts with
        | 5, 0, 4, 4
        | 4, 0, 5, 4
        | 4, 0, 4, 5 -> true
        | _ -> false
    shapeMatches && hand.Hcp >= 9<hcp> && hand.ControlCount = 4
(* TEMP-NMB: Only with void in hearts and 5 spades...
let private responderMatches (hand:Hand) =
    let shapeMatches =
        match hand.SuitCounts with
        | 5, 0, 4, 4 -> true
        | _ -> false
    shapeMatches && hand.Hcp >= 9<hcp> && hand.ControlCount = 4 *)
(* TEMP-NMB: Only with void in hearts or singleton Ah...
let private responderMatches (hand:Hand) =
    let shapeMatches =
        match hand.SuitCounts with
        | 4, 1, 4, 4 -> hand.Cards |> List.contains (Card.Make(Ace, Heart))
        | 5, 0, 4, 4
        | 4, 0, 5, 4
        | 4, 0, 4, 5 -> true
        | _ -> false
    shapeMatches && hand.Hcp >= 9<hcp> && hand.ControlCount = 4 *)

let private contracts = [ SixLevel, Suit Spade, false; SevenLevel, Suit Spade, false ]

let generate i : (int * Deal) option =
    let shuffledDeck, _ = Deck.MakeShuffled().Deal(CARDS_PER_HAND * 4)
    let nonOpenerCards = shuffledDeck |> List.filter (fun card -> opener.Cards |> List.contains card |> not)
    let responder = nonOpenerCards |> List.take CARDS_PER_HAND |> Hand.Make
    if responderMatches responder then
        let openerLHO = nonOpenerCards |> List.skip CARDS_PER_HAND |> List.take CARDS_PER_HAND |> Hand.Make
        let openerRHO = nonOpenerCards |> List.skip (CARDS_PER_HAND * 2) |> List.take CARDS_PER_HAND |> Hand.Make
        Some (i, Deal.Make(openerPosition, vulnerability, vulnerability, opener, openerLHO, responder, openerRHO))
    else None

let run count =
    if count = 0 then raise CountMustBeGreaterThanZeroException
    let countText = if count = 1 then "deal" else "deals"
    writeNewLine $"{exampleAuction} -> generating and analyzing {count} matching {countText}:\n" ConsoleColor.Magenta
    writeBlankLine()
    let generator = Seq.initInfinite generate |> Seq.choose id
    let mutable iMax = 0
    let mutable contractsResults = []
    let start = DateTime.UtcNow
    generator |> Seq.take count |> Seq.iter (fun (i, deal) ->
        iMax <- i + 1
        write "." ConsoleColor.Cyan
        (* TEMP-NMB: Output to check logic...
        writeBlankLine ()
        deal.Summary(false, false) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.Cyan) *)
        let results = calculateDoubleDummy deal
        let contractsResult =
            contracts
            |> List.map (fun (level, strain, openerIsDeclarer) ->
                let declarerPosition = if openerIsDeclarer then openerPosition else openerPosition.Partner
                let makes =
                    match results.Level(declarerPosition, strain) with
                    | Some level' when level' >= level -> true
                    | _ -> false
                (* TEMP-NMB: Output to check logic...
                writeNewLine $"{level.ShortText}{strain.ShortText} -> {makes}\n" ConsoleColor.Cyan *)
                makes)
        contractsResults <- contractsResult :: contractsResults)
    writeBlankLine()
    contracts |> List.iteri (fun i (level, strain, openerIsDeclarer) ->
        let declarer = if openerIsDeclarer then "opener" else "responder"
        let contract = $"{level.ShortText}{strain.ShortText} by {declarer}"
        let contractResults = contractsResults |> List.map(fun list -> list |> List.item i)
        let total = contractResults.Length
        let succeeded = contractResults |> List.filter id |> List.length
        let makePercentage = (100. * float succeeded) / float total
        writeNewLine $"{contract}: %0.2f{makePercentage}%%" ConsoleColor.Gray
    )
    writeBlankLine()
    writeNewLine $"{exampleAuction} -> generated and analyzed {count} matching {countText} (from {iMax} random deal/s) in %0.2f{(DateTime.UtcNow - start).TotalSeconds} seconds" ConsoleColor.DarkYellow
