module Aornota.BridgeSim.DevConsole.Program

open Aornota.BridgeSim.Common.SourcedLogger
open Aornota.BridgeSim.DevConsole.Console
open Aornota.BridgeSim.Domain.Auction
open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Formatting.Auction
open Aornota.BridgeSim.Domain.Formatting.Core
open Aornota.BridgeSim.Domain.Formatting.Deal
open Aornota.BridgeSim.Domain.Scoring.Auction

open Giraffe.SerilogExtensions
open Microsoft.Extensions.Configuration
open Serilog
open System
open System.IO

let [<Literal>] private SOURCE = "DevConsole.Program"

// TODO-NMB: Investigate why Release not writing to ...\logs?...

let private configuration =
    ConfigurationBuilder()
        .AddJsonFile("appsettings.json", false)
#if DEBUG
        .AddJsonFile("appsettings.development.json", false)
#else
        .AddJsonFile("appsettings.production.json", false)
#endif
        .Build()

do Log.Logger <- LoggerConfiguration().ReadFrom.Configuration(configuration).Destructure.FSharpTypes().CreateLogger()

let private sourcedLogger = Log.Logger |> sourcedLogger SOURCE

let private debugOrRelease =
#if DEBUG
    "Debug"
#else
    "Release"
#endif

//let rec private findSrcDir (currentDir:DirectoryInfo) = if currentDir.Name = "src" then currentDir.FullName else findSrcDir currentDir.Parent

let private random = Random()

let private mainAsync () = async {
    try
        writeNewLine "Running " ConsoleColor.Yellow
        write debugOrRelease ConsoleColor.DarkYellow
        write $" {SOURCE}.mainAsync\n" ConsoleColor.Yellow

        (* Deal (and diagram/s) stuff... *)
        writeNewLine "Testing Deal behaviour and diagram/s:\n\n" ConsoleColor.Magenta
        let randopmDealer () = match random.Next(4) with | 0 -> North | 1 -> East | 2 -> South | _ -> West
        let randomVulnerability () = match random.Next(2) with | 0 -> NotVulnerable | _ -> Vulnerable
        let deals = 3
        for n in 1..deals do
            let deal = Deal.Make(randopmDealer (), randomVulnerability (), randomVulnerability ())
            deal.Diagram(true) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.Cyan)
            //deal.Summary(true) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.DarkCyan)
            if n < deals then writeNewLine "\t-----\n\n" ConsoleColor.DarkMagenta

        (* Auction (and diagram) stuff...
        writeNewLine "Testing Auction behaviour and diagram:\n\n" ConsoleColor.Magenta
        let auction = Auction.Make(East)
        let auction = auction.Bid(East, Pass)
        let auction = auction.Bid(South, Bid (OneLevel, NoTrump)) // 12-14 balanced
        let auction = auction.Bid(West, Pass)
        let auction = auction.Bid(North, Bid (TwoLevel, Suit Heart)) // transfer to spades (5+ suit)
        let auction = auction.Bid(East, Bid.Double) // showing hearts (lead-directing?)
        let auction = auction.Bid(South, Bid (TwoLevel, Suit Spade)) // completing transfer
        let auction = auction.Bid(West, Pass)
        let auction = auction.Bid(North, Bid (ThreeLevel, NoTrump)) // offering a choice of games (pass with doubleton spades; else bid 4S)
        let auction = auction.Bid(East, Pass)
        let auction = auction.Bid(South, Pass) // doubleton spades
        let auction = auction.Bid(West, Bid.Double)
        let auction = auction.Bid(North, Pass)
        let auction = auction.Bid(East, Pass)
        let auction = auction.Bid(South, Pass)
        auction.Diagram |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.Cyan) *)

        (* Duplicate scoring stuff...
        writeNewLine "Testing duplicate scoring:\n\n" ConsoleColor.Magenta
        let contract, vulnerability, tricksTaken = Contract (TwoLevel, NoTrump, Undoubled, South), NotVulnerable, 8u
        writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(vulnerability, tricksTaken)}" ConsoleColor.Cyan
        let contract, vulnerability, tricksTaken = Contract (TwoLevel, NoTrump, Doubled, South), Vulnerable, 8u
        writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(vulnerability, tricksTaken)}" ConsoleColor.Cyan
        let contract, vulnerability, tricksTaken = Contract (TwoLevel, NoTrump, Undoubled, South), Vulnerable, 9u
        writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(vulnerability, tricksTaken)}" ConsoleColor.Cyan
        writeBlankLine ()
        let contract, vulnerability, tricksTaken = Contract (ThreeLevel, NoTrump, Undoubled, South), NotVulnerable, 9u
        writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(vulnerability, tricksTaken)}" ConsoleColor.Cyan
        let contract, vulnerability, tricksTaken = Contract (ThreeLevel, NoTrump, Undoubled, South), Vulnerable, 9u
        writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(vulnerability, tricksTaken)}" ConsoleColor.Cyan
        let contract, vulnerability, tricksTaken = Contract (ThreeLevel, NoTrump, Undoubled, South), Vulnerable, 10u
        writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(vulnerability, tricksTaken)}" ConsoleColor.Cyan
        writeBlankLine ()
        let contract, vulnerability, tricksTaken = Contract (TwoLevel, Suit Heart, Undoubled, South), NotVulnerable, 9u
        writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(vulnerability, tricksTaken)}" ConsoleColor.Cyan
        let contract, vulnerability, tricksTaken = Contract (TwoLevel, Suit Spade, Doubled, South), NotVulnerable, 8u
        writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(vulnerability, tricksTaken)}" ConsoleColor.Cyan
        let contract, vulnerability, tricksTaken = Contract (OneLevel, Suit Club, Redoubled, South), Vulnerable, 10u
        writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(vulnerability, tricksTaken)}" ConsoleColor.Cyan
        let contract, vulnerability, tricksTaken = Contract (FiveLevel, Suit Diamond, Undoubled, South), Vulnerable, 11u
        writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(vulnerability, tricksTaken)}" ConsoleColor.Cyan
        writeBlankLine ()
        let contract, vulnerability, tricksTaken = Contract (SixLevel, NoTrump, Undoubled, South), NotVulnerable, 12u
        writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(vulnerability, tricksTaken)}" ConsoleColor.Cyan
        let contract, vulnerability, tricksTaken = Contract (SixLevel, NoTrump, Doubled, South), Vulnerable, 13u
        writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(vulnerability, tricksTaken)}" ConsoleColor.Cyan
        let contract, vulnerability, tricksTaken = Contract (SevenLevel, NoTrump, Undoubled, South), Vulnerable, 13u
        writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(vulnerability, tricksTaken)}" ConsoleColor.Cyan
        writeBlankLine ()
        let contract, vulnerability, tricksTaken = Contract (ThreeLevel, NoTrump, Undoubled, South), NotVulnerable, 4u
        writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(vulnerability, tricksTaken)}" ConsoleColor.Cyan
        let contract, vulnerability, tricksTaken = Contract (ThreeLevel, NoTrump, Doubled, South), NotVulnerable, 4u
        writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(vulnerability, tricksTaken)}" ConsoleColor.Cyan
        let contract, vulnerability, tricksTaken = Contract (ThreeLevel, NoTrump, Redoubled, South), NotVulnerable, 4u
        writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(vulnerability, tricksTaken)}" ConsoleColor.Cyan
        let contract, vulnerability, tricksTaken = Contract (ThreeLevel, NoTrump, Doubled, South), Vulnerable, 5u
        writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(vulnerability, tricksTaken)}" ConsoleColor.Cyan
        let contract, vulnerability, tricksTaken = Contract (ThreeLevel, NoTrump, Redoubled, South), Vulnerable, 6u
        writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(vulnerability, tricksTaken)}" ConsoleColor.Cyan
        writeBlankLine () *)

        (* Deck stuff...
        writeNewLine "Testing Deck behaviour:\n\n" ConsoleColor.Magenta
        let deck = Deck.MakeShuffled()
        writeNewLine $"\tShuffled deck -> {deck.Text}\n" ConsoleColor.DarkCyan
        let firstSeatCards, deck = deck.Deal(13)
        let firstSeatHand = Hand.Make(firstSeatCards)
        writeNewLine $"\t{FirstSeat.ShortText} -> {firstSeatHand.Text}\n" ConsoleColor.Cyan
        writeNewLine $"\tRemaining deck -> {deck.Text}\n" ConsoleColor.DarkCyan
        let secondSeatCards, deck = deck.Deal(13)
        let secondSeatHand = Hand.Make(secondSeatCards)
        writeNewLine $"\t{SecondSeat.ShortText} -> {secondSeatHand.Text}\n" ConsoleColor.Cyan
        writeNewLine $"\tRemaining deck -> {deck.Text}\n" ConsoleColor.DarkCyan
        let thirdSeatCards, deck = deck.Deal(13)
        let thirdSeatHand = Hand.Make(thirdSeatCards)
        writeNewLine $"\t{ThirdSeat.ShortText} -> {thirdSeatHand.Text}\n" ConsoleColor.Cyan
        writeNewLine $"\tRemaining deck -> {deck.Text}\n" ConsoleColor.DarkCyan
        let fourthSeatCards, deck = deck.Deal(13)
        let fourthSeatHand = Hand.Make(fourthSeatCards)
        writeNewLine $"\t{FourthSeat.ShortText} -> {fourthSeatHand.Text}\n" ConsoleColor.Cyan
        writeNewLine $"\tRemaining deck -> {deck.Text}\n" ConsoleColor.DarkCyan *)

    with | exn -> sourcedLogger.Error("Unexpected error:\n\t{errorMessage}", exn.Message)

    writeNewLine "\nPress any key to exit..." ConsoleColor.Yellow
    Console.ReadKey () |> ignore
    writeBlankLine ()
    return 0 }

[<EntryPoint>]
let main _ =
    async {
        do! Async.SwitchToThreadPool ()
        return! mainAsync ()
    } |> Async.RunSynchronously
