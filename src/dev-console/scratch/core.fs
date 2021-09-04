module Aornota.BridgeSim.DevConsole.Scratch.Core

open Aornota.BridgeSim.Common.Console
open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Formatting.Core
open Aornota.BridgeSim.Domain.Formatting.Deal

open System

let deck () =
    let deckText (deck:Deck) = if deck.Count > 0 then deck.Text else "(empty)"
    writeNewLine "Testing Deck behaviour:\n" ConsoleColor.Magenta
    let deck = Deck.MakeShuffled()
    writeNewLine $"\tShuffled deck -> {deckText deck}\n" ConsoleColor.DarkCyan
    let firstSeatCards, deck = deck.Deal(13)
    let firstSeatHand = Hand.Make(firstSeatCards)
    writeNewLine $"\t{FirstSeat.ShortText} -> {firstSeatHand.Text}\n" ConsoleColor.Cyan
    writeNewLine $"\tRemaining deck -> {deckText deck}\n" ConsoleColor.DarkCyan
    let secondSeatCards, deck = deck.Deal(13)
    let secondSeatHand = Hand.Make(secondSeatCards)
    writeNewLine $"\t{SecondSeat.ShortText} -> {secondSeatHand.Text}\n" ConsoleColor.Cyan
    writeNewLine $"\tRemaining deck -> {deckText deck}\n" ConsoleColor.DarkCyan
    let thirdSeatCards, deck = deck.Deal(13)
    let thirdSeatHand = Hand.Make(thirdSeatCards)
    writeNewLine $"\t{ThirdSeat.ShortText} -> {thirdSeatHand.Text}\n" ConsoleColor.Cyan
    writeNewLine $"\tRemaining deck -> {deckText deck}\n" ConsoleColor.DarkCyan
    let fourthSeatCards, deck = deck.Deal(13)
    let fourthSeatHand = Hand.Make(fourthSeatCards)
    writeNewLine $"\t{FourthSeat.ShortText} -> {fourthSeatHand.Text}\n" ConsoleColor.Cyan
    writeNewLine $"\tRemaining deck -> {deckText deck}\n" ConsoleColor.DarkCyan
