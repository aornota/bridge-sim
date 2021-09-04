module Aornota.BridgeSim.DevConsole.Scratch.Scenario

open Aornota.BridgeSim.Common.Console
open Aornota.BridgeSim.Dds.Interop.Dds
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

type private DealAndContracts = { Deal : Deal ; Contracts : Contract list ; Results : DoubleDummyResult list } with
    static member FromJson json = match Decode.Auto.fromString<DealAndContracts> json with | Ok value -> value | Error error -> raise (CannotDeserializeDealAndContractsException (json, error))
    member this.ToJson = Encode.Auto.toString<DealAndContracts> (4, this)

type Mode = | DisplayOnly of interactive:bool | SaveOnly | DisplayAndSave of interactive:bool with
    member this.Display = match this with | DisplayOnly _ | DisplayAndSave _ -> true | SaveOnly -> false
    member this.Save = match this with | SaveOnly | DisplayAndSave _ -> true | DisplayOnly _ -> false
    member this.Interactive = match this with | DisplayOnly true | DisplayAndSave true -> true | DisplayOnly false | SaveOnly | DisplayAndSave false -> false

let private scenarioDir scenarioName =
    let rec findSrcDir (currentDir:DirectoryInfo) = if currentDir.Name = "src" then currentDir.FullName else findSrcDir currentDir.Parent
    let dir = Path.Combine(findSrcDir (DirectoryInfo(Environment.CurrentDirectory)), "dev-console", "simulations", scenarioName)
    if not (Directory.Exists dir) then Directory.CreateDirectory dir |> ignore
    dir

let workInProgress (mode:Mode) withDoubleDummy count =
    let generate i = // deals where contract will probably be 4M but opener could choose 3NT - even though 5[+]-3 (or 6[+]-2) major fit is known
        let some i deal contracts = Some (i, { Deal = deal ; Contracts = contracts ; Results = [] })
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

                            // TODO-NMB: Temporarily change more common Some(s) to None, i.e. to check that rarer cases (5[+]-5[+] &c.) are handled correctly?...

                            match partnerSpades, partnerHearts, openerSpades, openerHearts with
                            | _, _, _, _ when partnerSpades >= 8 || partnerHearts >= 8 -> None // assume will always play in major with 8-card or better suit
                            // TODO-NMB: If exactly 6-4 in majors, handle as per 5-4 (but assuming correction to 4M if opener bids 3NT)?...
                            | _, _, _, _ when partnerSpades >= 6 && partnerHearts <= 4 -> None // assume will always play in spades with 6-card or better suit and fewer than 5 hearts
                            // TODO-NMB: What if 7-4 | 7-5 | 7-6 in majors?...
                            | _, _, _, _ when partnerSpades <= 5 && partnerHearts >= 6 -> None // assume will always play in hearts with 6-card or better suit and fewer than 6 spades
                            // TODO-NMB: Treat 1NT | 2D | 2H | 3S as 6[+]-5[+] in majors (but not necessarily longer spades)?...
                            // Transfer sequence: 1NT | 2D | 2H | 3S shows game-force with 6+ spades and 5+ hearts.
                            | _, _, _, _ when partnerSpades >= 6 && partnerHearts >= 5 && openerSpades >= 3 -> None // assume opener chooses 4S with at least 9-card fit
                            | _, _, _, _ when partnerSpades >= 6 && partnerHearts >= 5 && openerHearts >= 4 -> None // assume opener chooses 4H with at least 9-card fit
                            | _, _, 2, 3 when partnerSpades >= 6 && partnerHearts >= 5 -> // opener chooses 4S (6[+]-2 fit), 4H (5[+]-3 fit) or 3NT; responder is declarer if 4S
                                some i deal [ contract FourLevel (Suit Spade) position.Partner ; contract FourLevel (Suit Heart) position ; contract ThreeLevel NoTrump position ]
                            | _, _, 2, _ when partnerSpades >= 6 && partnerHearts >= 5 -> None // assume opener chooses 4S with 6(+)-2 fit
                            | _, _, _, _ when partnerSpades >= 6 && partnerHearts >= 5 -> None // should never happen: opener cannot have fewer than 2 spades when balanced
                            // Transfer sequence: 1NT | 2H | 2S | 3H shows game-force with exactly 5-5 in majors.
                            | 5, 5, _, _ when openerSpades >= 4 || openerHearts >= 4 -> None // assume opener chooses 4M with at least 9-card fit
                            | 5, 5, 3, 3 -> // opener chooses 4S (5-3 fit), 4H (5-3 fit) or 3NT; responder is declarer if 4H
                                some i deal [ contract FourLevel (Suit Spade) position ; contract FourLevel (Suit Heart) position.Partner ; contract ThreeLevel NoTrump position ]
                            | 5, 5, 3, _ -> // opener chooses 4S (5-3 fit) or 3NT
                                some i deal [ contract FourLevel (Suit Spade) position ; contract ThreeLevel NoTrump position ]
                            | 5, 5, _, 3 -> // opener chooses 4H (5-3 fit) or 3NT; responder is declarer if 4H
                                some i deal [ contract FourLevel (Suit Heart) position.Partner ; contract ThreeLevel NoTrump position ]
                            | 5, 5, _, _ -> None // should never happen: opener cannot have fewer than 3 spades and fewer than 3 hearts when balanced
                            // Stayman sequence (when opener has no 4-card or better major): 1NT | 2C | 2D | [3S or 3H] shows game-force with exactly [5=4 or 4=5] in majors.
                            | 5, 4, _, _ when openerSpades >= 4 || openerHearts >= 4 -> None // assume respnder chooses 4M once Stayman reveals at least 8-card fit
                            | 5, 4, 3, _ -> // opener chooses 4S (5-3 fit) or 3NT; responder is declarer if 4S
                                some i deal [ contract FourLevel (Suit Spade) position.Partner ; contract ThreeLevel NoTrump position ]
                            | 5, 4, _, _ -> None // assume opener (must be 2=3 in majors when balanced) chooses 3NT
                            | 4, 5, _, _ when openerSpades >= 4 || openerHearts >= 4 -> None // assume respnder chooses 4M once Stayman reveals at least 8-card fit
                            | 4, 5, _, 3 -> // opener chooses 4H (5-3 fit) or 3NT; responder is declarer if 4H
                                some i deal [ contract FourLevel (Suit Heart) position.Partner ; contract ThreeLevel NoTrump position ]
                            | 4, 5, _, _ -> None // assume opener (must be 3=2 in majors when balanced) chooses 3NT
                            // Transfer sequence: 1NT | [2H or 2D] | [2S or 2H] | 3NT shows game-force with exactly 5 [spades or hearts].
                            | 5, _, _, _ when openerSpades >= 4 -> None // assume opener chooses 4S with at least 9-card fit
                            | 5, _, 3, _ -> // opener chooses 4S (5-3 fit) or 3NT
                                some i deal [ contract FourLevel (Suit Spade) position ; contract ThreeLevel NoTrump position ]
                            | 5, _, _, _ -> None // assume opener (must have 2 spades when balanced) chooses 3NT
                            | _, 5, _, _ when openerHearts >= 4 -> None // assume opener chooses 4H with at least 9-card fit
                            | _, 5, _, 3 -> // opener chooses 4H (5-3 fit) or 3NT
                                some i deal [ contract FourLevel (Suit Heart) position ; contract ThreeLevel NoTrump position ]
                            | _, _, 5, _ -> None // assume opener (must have 2 hearts when balanced) chooses 3NT
                            // Other sequences (including Stayman when responder is not 5=4 or 4=5 in majors) will not offer a choice of contracts.
                            | _ -> None
                    | _ -> None
                | _ -> check deal remaining
        check (Deal.MakeRandom()) [ North ; East; South ; West ]
    writeNewLine "Generating deals of interest:\n\n" ConsoleColor.Magenta
    let generator = Seq.initInfinite generate |> Seq.choose id
    let mutable iMax = 0
    let subDir = "WIP"
    let start = DateTime.UtcNow
    generator |> Seq.take count |> Seq.iter (fun (i, dealAndContracts) ->
        iMax <- i
        if mode.Display then
            writeBlankLine ()
            dealAndContracts.Deal.Summary(true) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.Cyan)
            let contractsText = dealAndContracts.Contracts |> List.map (fun contract -> contract.ShortText) |> String.concat " | "
            writeNewLine "\tContracts of interest: " ConsoleColor.DarkGray
            write $"{contractsText}\n" ConsoleColor.Gray
        let dealAndContracts = if withDoubleDummy then { dealAndContracts with Results = calculateDoubleDummy dealAndContracts.Deal } else dealAndContracts
        if mode.Display && withDoubleDummy then
            Dds.writeDoubleDummyResults dealAndContracts.Results
            writeBlankLine ()
        if mode.Save then
            let fileName = $"{Guid.NewGuid()}.json"
            File.WriteAllText(Path.Combine(scenarioDir subDir, fileName), dealAndContracts.ToJson)
            if mode.Display then writeNewLine $"\tSaved to .../{subDir}/{fileName}\n" ConsoleColor.DarkCyan
        if mode.Interactive then
            writeNewLine "Press any key to continue..." ConsoleColor.DarkYellow
            Console.ReadKey () |> ignore
            writeBlankLine ()
        else if not mode.Display then write "." ConsoleColor.DarkGray)
    let conditionalText = if withDoubleDummy then "Generated and analyzed" else "Generated"
    if mode.Display then writeNewLine $"\n{conditionalText} {count} matching deal/s (from {iMax} random deal/s)" ConsoleColor.Gray
    else
        let elapsed = (DateTime.UtcNow - start).TotalSeconds
        writeNewLine $"\n{conditionalText} {count} matching deal/s (from {iMax} random deal/s) in {elapsed} seconds" ConsoleColor.Gray
