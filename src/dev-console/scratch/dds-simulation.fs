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
open Aornota.BridgeSim.Domain.Random.Deal
open Aornota.BridgeSim.Domain.Simulation.ComparisonConstraint
open Aornota.BridgeSim.Domain.Simulation.HandScenario
open Aornota.BridgeSim.Domain.Simulation.PartnershipScenario
open Aornota.BridgeSim.Domain.Simulation.Scenario

open System

let private openerPosition, vulnerability = North, NotVulnerable // arbitrary

let private scenario = "1C-1S-1NT-2H-2S-3H-6S (declared by responder)"

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

let private responderMatches (hand:Hand) =
    let shapeMatches =
        match hand.SuitCounts with
        | 4, 1, 4, 4
        | 5, 0, 4, 4
        | 4, 0, 5, 4
        | 4, 0, 4, 5 -> true
        | _ -> false
    shapeMatches && hand.Hcp >= 9<hcp> && hand.Cc = 4<cc>
(* TEMP-NMB: Only with void in hearts...
let private responderMatches (hand:Hand) =
    let shapeMatches =
        match hand.SuitCounts with
        | 5, 0, 4, 4
        | 4, 0, 5, 4
        | 4, 0, 4, 5 -> true
        | _ -> false
    shapeMatches && hand.Hcp >= 9<hcp> && hand.Cc = 4<cc> *)
(* TEMP-NMB: Only with void in hearts and 5 spades...
let private responderMatches (hand:Hand) =
    let shapeMatches =
        match hand.SuitCounts with
        | 5, 0, 4, 4 -> true
        | _ -> false
    shapeMatches && hand.Hcp >= 9<hcp> && hand.Cc = 4<cc> *)
(* TEMP-NMB: Only with void in hearts or singleton Ah...
let private responderMatches (hand:Hand) =
    let shapeMatches =
        match hand.SuitCounts with
        | 4, 1, 4, 4 -> hand.Cards |> List.contains (Card.Make(Ace, Heart))
        | 5, 0, 4, 4
        | 4, 0, 5, 4
        | 4, 0, 4, 5 -> true
        | _ -> false
    shapeMatches && hand.Hcp >= 9<hcp> && hand.Cc = 4<cc> *)

let private contracts = [ SixLevel, NoTrump, false; SixLevel, Suit Spade, false; SevenLevel, NoTrump, false; SevenLevel, Suit Spade, false ]

// TEMP-NMB...let private scenario = "1C-1S... when opener is 3=3=3=4 and responder is 4=4=5=0 (25-29 total HCP)"

// TEMP-NMB...let private contracts = [ ThreeLevel, NoTrump, true; FourLevel, Suit Spade, false; FourLevel, Suit Heart, true ; FiveLevel, Suit Diamond, true ]

let generateForOpener i : (int * Deal) option =
    let shuffledDeck, _ = Deck.MakeShuffled().Deal(CARDS_PER_HAND * 4)
    let nonOpenerCards = shuffledDeck |> List.filter (fun card -> opener.Cards |> List.contains card |> not)
    let responder = nonOpenerCards |> List.take CARDS_PER_HAND |> Hand.Make
    if responderMatches responder then
        let openerLHO = nonOpenerCards |> List.skip CARDS_PER_HAND |> List.take CARDS_PER_HAND |> Hand.Make
        let openerRHO = nonOpenerCards |> List.skip (CARDS_PER_HAND * 2) |> List.take CARDS_PER_HAND |> Hand.Make
        Some (i, Deal.Make(openerPosition, vulnerability, vulnerability, opener, openerLHO, responder, openerRHO))
    else None

let generateForOpener4333 i : (int * Deal) option =
    let deal = Deal.MakeRandom()
    let opener = deal.Hand(openerPosition)
    match opener.SuitCounts, opener.Hcp with
    | (3, 3, 3, 4), openerHcp when openerHcp >= 16<hcp> ->
        let responder = deal.Hand(openerPosition.Partner)
        match responder.SuitCounts, responder.Hcp with
        | (4, 4, 5, 0), responderHcp when responderHcp >= 9<hcp> && openerHcp + responderHcp <= 29<hcp> -> Some (i, deal)
        | _ -> None
    | _ -> None

let run count =
    if count = 0 then raise CountMustBeGreaterThanZeroException
    let countText = if count = 1 then "deal" else "deals"
    writeNewLine $"{scenario} -> generating and analyzing {count} matching {countText}:\n" ConsoleColor.Magenta
    writeBlankLine()
    let generator = Seq.initInfinite generateForOpener |> Seq.choose id
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
    writeNewLine $"{scenario} -> generated and analyzed {count} matching {countText} (from {iMax} random deal/s) in %0.2f{(DateTime.UtcNow - start).TotalSeconds} seconds" ConsoleColor.DarkYellow

let wip () =
    writeNewLine "Work-in-progress for scenario {...} computation expressions...\n" ConsoleColor.Magenta
    let north =
        handScenario {
            position North
            hcp (HandHcpConstraint.Between (18, 25))
            cc (CcConstraint.AtLeast 4)
            //shape (ShapeConstraint.ShapeCategories [ Balanced; SemiBalanced ])
            //shape (ShapeConstraint.Shapes [ EightTwoTwoOne; EightThreeOneOne; EightThreeTwoZero; EightFourOneZero; EightFiveZeroZero ])
            //shape (ShapeConstraint.SuitCounts [ (4, 1, 4, 4); (5, 0, 4, 4); (4, 0, 5, 4); (4, 0, 4, 5) ])
            shape (ShapeConstraint.SuitConstraints (Some (SuitConstraint.AtLeast 4), Some (SuitConstraint.AtMost 5), Some (SuitConstraint.Between (1, 4)), Some (SuitConstraint.Exactly 2)))
            cards [
                Ace, Spade
                Queen, Spade
                Seven, Spade
                Four, Spade
                King, Heart
                Eight, Heart
                Five, Heart
                Three, Heart
                Two, Heart
                Ace, Diamond
                Jack, Diamond
                King, Club
                Queen, Club ]
            //customPredicate (fun _ -> false)
        }
    writeNewLine north.Text ConsoleColor.Cyan
    writeBlankLine()
    let northSouth =
        partnershipScenario {
            partnership NorthSouth
            hcp (PartnershipHcpConstraint.AtMost 29)
            cc (CcConstraint.AtMost 9)
            shape (PartnershipShapeConstraint.SuitCounts [ (9, 6, 6, 5); (10, 6, 6, 4) ])
        }
    writeNewLine northSouth.Text ConsoleColor.Cyan
