module Aornota.BridgeSim.DevConsole.Scratch.Auction

open Aornota.BridgeSim.Common.Console
open Aornota.BridgeSim.Domain.Auction
open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Scoring.Auction
open Aornota.BridgeSim.Domain.Formatting.Auction
open Aornota.BridgeSim.Domain.Formatting.Core

open System

let auctionAndDiagram () =
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
    auction.Diagram |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.Cyan)

let duplicateScoring () =
    writeNewLine "Testing duplicate scoring:\n\n" ConsoleColor.Magenta
    let contract = PassedOut
    writeNewLine $"{contract.ShortText} -> {contract.DuplicateScore(None)}" ConsoleColor.Cyan
    writeBlankLine ()
    let contract, vulnerability, tricksTaken = Contract (TwoLevel, NoTrump, Undoubled, South), NotVulnerable, 8u
    writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(Some (vulnerability, tricksTaken))}" ConsoleColor.Cyan
    let contract, vulnerability, tricksTaken = Contract (TwoLevel, NoTrump, Doubled, South), Vulnerable, 8u
    writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(Some (vulnerability, tricksTaken))}" ConsoleColor.Cyan
    let contract, vulnerability, tricksTaken = Contract (TwoLevel, NoTrump, Undoubled, South), Vulnerable, 9u
    writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(Some (vulnerability, tricksTaken))}" ConsoleColor.Cyan
    writeBlankLine ()
    let contract, vulnerability, tricksTaken = Contract (ThreeLevel, NoTrump, Undoubled, South), NotVulnerable, 9u
    writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(Some (vulnerability, tricksTaken))}" ConsoleColor.Cyan
    let contract, vulnerability, tricksTaken = Contract (ThreeLevel, NoTrump, Undoubled, South), Vulnerable, 9u
    writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(Some (vulnerability, tricksTaken))}" ConsoleColor.Cyan
    let contract, vulnerability, tricksTaken = Contract (ThreeLevel, NoTrump, Undoubled, South), Vulnerable, 10u
    writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(Some (vulnerability, tricksTaken))}" ConsoleColor.Cyan
    writeBlankLine ()
    let contract, vulnerability, tricksTaken = Contract (TwoLevel, Suit Heart, Undoubled, South), NotVulnerable, 9u
    writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(Some (vulnerability, tricksTaken))}" ConsoleColor.Cyan
    let contract, vulnerability, tricksTaken = Contract (TwoLevel, Suit Spade, Doubled, South), NotVulnerable, 8u
    writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(Some (vulnerability, tricksTaken))}" ConsoleColor.Cyan
    let contract, vulnerability, tricksTaken = Contract (OneLevel, Suit Club, Redoubled, South), Vulnerable, 10u
    writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(Some (vulnerability, tricksTaken))}" ConsoleColor.Cyan
    let contract, vulnerability, tricksTaken = Contract (FiveLevel, Suit Diamond, Undoubled, South), Vulnerable, 11u
    writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(Some (vulnerability, tricksTaken))}" ConsoleColor.Cyan
    writeBlankLine ()
    let contract, vulnerability, tricksTaken = Contract (SixLevel, NoTrump, Undoubled, South), NotVulnerable, 12u
    writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(Some (vulnerability, tricksTaken))}" ConsoleColor.Cyan
    let contract, vulnerability, tricksTaken = Contract (SixLevel, NoTrump, Doubled, South), Vulnerable, 13u
    writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(Some (vulnerability, tricksTaken))}" ConsoleColor.Cyan
    let contract, vulnerability, tricksTaken = Contract (SevenLevel, NoTrump, Undoubled, South), Vulnerable, 13u
    writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(Some (vulnerability, tricksTaken))}" ConsoleColor.Cyan
    writeBlankLine ()
    let contract, vulnerability, tricksTaken = Contract (ThreeLevel, NoTrump, Undoubled, South), NotVulnerable, 4u
    writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(Some (vulnerability, tricksTaken))}" ConsoleColor.Cyan
    let contract, vulnerability, tricksTaken = Contract (ThreeLevel, NoTrump, Doubled, South), NotVulnerable, 4u
    writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(Some (vulnerability, tricksTaken))}" ConsoleColor.Cyan
    let contract, vulnerability, tricksTaken = Contract (ThreeLevel, NoTrump, Redoubled, South), NotVulnerable, 4u
    writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(Some (vulnerability, tricksTaken))}" ConsoleColor.Cyan
    let contract, vulnerability, tricksTaken = Contract (ThreeLevel, NoTrump, Doubled, South), Vulnerable, 5u
    writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(Some (vulnerability, tricksTaken))}" ConsoleColor.Cyan
    let contract, vulnerability, tricksTaken = Contract (ThreeLevel, NoTrump, Redoubled, South), Vulnerable, 6u
    writeNewLine $"{contract.ShortText} when {vulnerability.TextLower} taking {tricksTaken} trick/s -> {contract.DuplicateScore(Some (vulnerability, tricksTaken))}" ConsoleColor.Cyan
    writeBlankLine ()
