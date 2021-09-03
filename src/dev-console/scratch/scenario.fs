module Aornota.BridgeSim.DevConsole.Scratch.Scenario

open Aornota.BridgeSim.Common.Console
open Aornota.BridgeSim.Domain.Auction
open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Evaluation.Core
open Aornota.BridgeSim.Domain.Formatting.Auction
open Aornota.BridgeSim.Domain.Formatting.Deal
open Aornota.BridgeSim.Domain.Random.Deal

open System
open System.IO
open Thoth.Json.Net

exception CannotDeserializeDealAndContractsException of json:string * error:string

type private DealAndContracts = { Deal : Deal ; Contracts : Contract list } with
    static member FromJson json = match Decode.Auto.fromString<DealAndContracts> json with | Ok value -> value | Error error -> raise (CannotDeserializeDealAndContractsException (json, error))
    member this.ToJson = Encode.Auto.toString<DealAndContracts> (4, this)

type Mode = | DisplayOnly | SaveOnly | SaveAndDisplay with
    member this.Display = this <> SaveOnly
    member this.Save = this <> DisplayOnly

let private scenarioDir scenarioName =
    let rec findSrcDir (currentDir:DirectoryInfo) = if currentDir.Name = "src" then currentDir.FullName else findSrcDir currentDir.Parent
    let dir = Path.Combine(findSrcDir (DirectoryInfo(Environment.CurrentDirectory)), "dev-console", "simulations", scenarioName)
    if not (Directory.Exists dir) then Directory.CreateDirectory dir |> ignore
    dir

let workInProgress (mode:Mode) =
    let generate _ = // deals where contract will probably be 4M but opener could choose 3NT - even though 5[+]-3 (or 6[+]-2) major fit is known
        let rec check (deal:Deal) positions =
            let contract level strain position = Contract (level, strain, Undoubled, position)
            match positions with
            | [] -> None
            | position :: remaining ->
                let hand = deal.Hand(position)
                match hand.ShapeCategory, hand.Hcp with
                | Balanced, hcp when hcp >= 12<hcp> && hcp <= 14<hcp> -> // 1NT opener
                    let openerSpades, openerHearts, _, _ = hand.SuitCounts
                    let partnerHand = deal.Hand(position.Partner)
                    match hcp + partnerHand.Hcp with
                    | combinedHcp when combinedHcp >= 24<hcp> && combinedHcp <= 29<hcp> -> // game-ish (but probably not slam-ish) combined strength
                        let partnerSpades, partnerHearts, partnerDiamonds, partnerClubs = partnerHand.SuitCounts
                        if partnerDiamonds >= 7 || partnerClubs >= 7 then None // assume will always play in minor (or 3NT) with 7-card or better suit
                        else
                            match partnerSpades, partnerHearts, openerSpades, openerHearts with
                            | _, _, _, _ when partnerSpades >= 8 || partnerHearts >= 8 -> None // assume will always play in major with 8-card or better suit
                            | _, _, _, _ when partnerSpades >= 6 && partnerHearts <= 4 -> None // assume will always play in spades with 6-card or better suit and fewer than 5 hearts
                            | _, _, _, _ when partnerSpades <= 5 && partnerHearts >= 6 -> None // assume will always play in hearts with 6-card or better suit and fewer than 6 spades
                            // Transfer sequence: 1NT | 2D | 2H | 3S shows game-force with 6+ spades and 5+ hearts.
                            | _, _, _, _ when partnerSpades >= 6 && partnerHearts >= 5 && openerSpades >= 3 -> None // assume opener chooses 4S with at least 9-card fit
                            | _, _, _, _ when partnerSpades >= 6 && partnerHearts >= 5 && openerHearts >= 4 -> None // assume opener chooses 4H with at least 9-card fit
                            | _, _, 2, 3 when partnerSpades >= 6 && partnerHearts >= 5 -> // opener chooses 4S (6[+]-2 fit), 4H (5[+]-3 fit) or 3NT; responder is declarer if 4S
                                Some (deal, [ contract FourLevel (Suit Spade) position.Partner ; contract FourLevel (Suit Heart) position ; contract ThreeLevel NoTrump position ])
                            | _, _, 2, _ when partnerSpades >= 6 && partnerHearts >= 5 -> None // assume opener chooses 4S with 6(+)-2 fit
                            | _, _, _, _ when partnerSpades >= 6 && partnerHearts >= 5 -> None // should never happen: opener cannot have fewer than 2 spades when balanced
                            // Transfer sequence: 1NT | 2H | 2S | 3H shows game-force with exactly 5-5 in majors.
                            | 5, 5, _, _ when openerSpades >= 4 || openerHearts >= 4 -> None // assume opener chooses 4M with at least 9-card fit
                            | 5, 5, 3, 3 -> // opener chooses 4S (5-3 fit), 4H (5-3 fit) or 3NT; responder is declarer if 4H
                                Some (deal, [ contract FourLevel (Suit Spade) position ; contract FourLevel (Suit Heart) position.Partner ; contract ThreeLevel NoTrump position ])
                            | 5, 5, 3, _ -> // opener chooses 4S (5-3 fit) or 3NT
                                Some (deal, [ contract FourLevel (Suit Spade) position ; contract ThreeLevel NoTrump position ])
                            | 5, 5, _, 3 -> // opener chooses 4H (5-3 fit) or 3NT; responder is declarer if 4H
                                Some (deal, [ contract FourLevel (Suit Heart) position.Partner ; contract ThreeLevel NoTrump position ])
                            | 5, 5, _, _ -> None // should never happen: opener cannot have fewer than 3 spades and fewer than 3 hearts when balanced
                            // Stayman sequence (when opener has no 4-card or better major): 1NT | 2C | 2D | [3S or 3H] shows game-force with exactly [5=4 or 4=5] in majors.
                            | 5, 4, _, _ when openerSpades >= 4 || openerHearts >= 4 -> None // assume respnder chooses 4M once Stayman reveals at least 8-card fit
                            | 5, 4, 3, _ -> // opener chooses 4S (5-3 fit) or 3NT; responder is declarer if 4S
                                Some (deal, [ contract FourLevel (Suit Spade) position.Partner ; contract ThreeLevel NoTrump position ])
                            | 5, 4, _, _ -> None // assume opener (must be 2=3 in majors when balanced) chooses 3NT
                            | 4, 5, _, _ when openerSpades >= 4 || openerHearts >= 4 -> None // assume respnder chooses 4M once Stayman reveals at least 8-card fit
                            | 4, 5, _, 3 -> // opener chooses 4H (5-3 fit) or 3NT; responder is declarer if 4H
                                Some (deal, [ contract FourLevel (Suit Heart) position.Partner ; contract ThreeLevel NoTrump position ])
                            | 4, 5, _, _ -> None // assume opener (must be 3=2 in majors when balanced) chooses 3NT
                            // Transfer sequence: 1NT | [2H or 2D] | [2S or 2H] | 3NT shows game-force with exactly 5 [spades or hearts].
                            | 5, _, _, _ when openerSpades >= 4 -> None // assume opener chooses 4S with at least 9-card fit
                            | 5, _, 3, _ -> // opener chooses 4S (5-3 fit) or 3NT
                                Some (deal, [ contract FourLevel (Suit Spade) position ; contract ThreeLevel NoTrump position ])
                            | 5, _, _, _ -> None // assume opener (must have 2 spades when balanced) chooses 3NT
                            | _, 5, _, _ when openerHearts >= 4 -> None // assume opener chooses 4H with at least 9-card fit
                            | _, 5, _, 3 -> // opener chooses 4H (5-3 fit) or 3NT
                                Some (deal, [ contract FourLevel (Suit Heart) position ; contract ThreeLevel NoTrump position ])
                            | _, _, 5, _ -> None // assume opener (must have 2 hearts when balanced) chooses 3NT
                            // Other sequences (including Stayman when responder is not 5=4 or 4=5 in majors) will not offer a choice of contracts.
                            | _ -> None
                    | _ -> None
                | _ -> check deal remaining
        check (Deal.MakeRandom()) [ North ; East; South ; West ]
    writeNewLine "Generating deals of interest:\n\n" ConsoleColor.Magenta
    let generator = Seq.initInfinite generate |> Seq.choose (Option.map (fun (deal, contracts) -> { Deal = deal ; Contracts = contracts }))
    let count, start = 100, DateTime.UtcNow
    generator |> Seq.take count |> Seq.iter (fun dealAndContracts ->
        if mode.Display then
            writeBlankLine ()
            dealAndContracts.Deal.Summary(true) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.Cyan)
            let contractsText = dealAndContracts.Contracts |> List.map (fun contract -> contract.ShortText) |> String.concat " | "
            writeNewLine $"\t{contractsText}\n" ConsoleColor.Gray
        if mode.Save then
            let file = Path.Combine(scenarioDir "work-in-progress", $"{Guid.NewGuid()}.json")
            File.WriteAllText(file, dealAndContracts.ToJson)
            if mode.Display then writeNewLine $"\tSaved to {file}\n" ConsoleColor.DarkCyan
        if mode.Display then
            writeNewLine "Press any key to continue..." ConsoleColor.DarkYellow
            Console.ReadKey () |> ignore
            writeBlankLine ()
        else write "." ConsoleColor.DarkGray)
    let elapsed = (DateTime.UtcNow - start).TotalSeconds
    writeNewLine $"\nGenerated {count} deal/s in {elapsed} seconds" ConsoleColor.Gray
