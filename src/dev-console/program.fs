module Aornota.BridgeSim.DevConsole.Program

open Aornota.BridgeSim.Common.SourcedLogger
open Aornota.BridgeSim.DevConsole.Console
open Aornota.BridgeSim.Domain.Auction
open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal

open Giraffe.SerilogExtensions
open Microsoft.Extensions.Configuration
open Serilog
open System
//open System.IO

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

let private mainAsync () = async {
    try
        writeNewLine "Running " ConsoleColor.Yellow
        write debugOrRelease ConsoleColor.DarkYellow
        write $" {SOURCE}.mainAsync\n" ConsoleColor.Yellow

        (* Deal stuff... *)
        writeNewLine "Testing Deal behaviour:\n\n" ConsoleColor.Magenta
        let deal = Deal.Make(South, NotVulnerable, NotVulnerable)
        let _, northHand = deal.SeatAndHand(North)
        let _, eastHand = deal.SeatAndHand(East)
        let _, southHand = deal.SeatAndHand(South)
        let _, westHand = deal.SeatAndHand(West)
        writeNewLine $"\tDealer -> {deal.Dealer.Text}\n" ConsoleColor.DarkCyan
        writeNewLine $"\t{North.Text} -> {northHand.Text}\n" ConsoleColor.Cyan
        writeNewLine $"\t{East.Text} -> {eastHand.Text}\n" ConsoleColor.Cyan
        writeNewLine $"\t{South.Text} -> {southHand.Text}\n" ConsoleColor.Cyan
        writeNewLine $"\t{West.Text} -> {westHand.Text}\n" ConsoleColor.Cyan

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

        (* Auction stuff...
        writeNewLine "Testing Auction behaviour:\n\n" ConsoleColor.Magenta
        let auction = Auction.Make(East)
        let auction = auction.Bid(East, Pass)
        let auction = auction.Bid(South, Bid (OneLevel, NoTrump)) // 12-14 balanced
        let auction = auction.Bid(West, Pass)
        let auction = auction.Bid(North, Bid (TwoLevel, Suit Heart)) // transfer to spades (5+ suit)
        let auction = auction.Bid(East, Bid.Double) // showing hearts
        let auction = auction.Bid(South, Bid (TwoLevel, Suit Spade)) // completing transfer
        let auction = auction.Bid(West, Pass)
        let auction = auction.Bid(North, Bid (ThreeLevel, NoTrump)) // offering a choice of games (pass with doubleton spades; else bid 4S)
        let auction = auction.Bid(East, Pass)
        let auction = auction.Bid(South, Pass) // doubleton spades
        let auction = auction.Bid(West, Pass)
        auction.Bids |> List.iter (fun (position, bid) -> write $"\t{position.ShortText} -> {bid.ShortText}\n" ConsoleColor.DarkCyan)
        match auction.State with
        | Completed contract -> writeNewLine $"\tAuction completed -> contract is {contract.ShortText}" ConsoleColor.Cyan
        | AwaitingBid (position, _) -> writeNewLine $"\tAuction in progress -> awaiting bid from {position.Text}" ConsoleColor.Cyan *)

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
