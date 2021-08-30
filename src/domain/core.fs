module Aornota.BridgeSim.Domain.Core

open Aornota.BridgeSim.Common.Mathy

type Rank = | Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two
    with
    member this.Hcp = match this with | Ace -> 4 | King -> 3 | Queen -> 2 | Jack -> 1 | _ -> 0
    member this.Text = match this with | Ace -> "Ace" | King -> "King" | Queen -> "Queen" | Jack -> "Jack" | Ten -> "Ten" | Nine -> "Nine" | Eight -> "Eight" | Seven -> "Seven" | Six -> "Six" | Five -> "Five" | Four -> "Four" | Three -> "Three" | Two -> "Two"
    member this.ShortText = match this with | Ace -> 'A' | King -> 'K' | Queen -> 'Q' | Jack -> 'J' | Ten -> 'T' | Nine -> '9' | Eight -> '8' | Seven -> '7' | Six -> '6' | Five -> '5' | Four -> '4' | Three -> '3' | Two -> '2'

type Suit = | Spade | Heart | Diamond | Club
    with
    member this.Rank = match this with | Spade -> 4 | Heart -> 3 | Diamond -> 2 | Club -> 1
    member this.Text = match this with | Spade -> "Spade" | Heart -> "Heart" | Diamond -> "Diamond" | Club -> "Club"
    member this.ShortTextLower = match this with | Spade -> "s" | Heart -> "h" | Diamond -> "d" | Club -> "c"
    member this.ShortTextUpper = match this with | Spade -> "S" | Heart -> "H" | Diamond -> "D" | Club -> "C"
    member this.Symbol = match this with | Spade -> "♠" | Heart -> "♥" | Diamond -> "♦" | Club -> "♣"

type Card = Rank * Suit

type CardS = Set<Card> // use Set to prevent duplicates
type Hand = CardS

type CardL = Card list // use list when ordering matters
type Deck = CardL

type Position = | North | East | South | West
    with
    member this.LHO = match this with | North -> East | East -> South | South -> West | West -> North
    member this.Partner = match this with | North -> South | East -> West | South -> North | West -> East
    member this.IsPartner(position) = this.Partner = position
    member this.IsPartnership(position) = this = position || this.IsPartner(position)
    member this.IsOpponent(position) = not (this.IsPartnership(position))
    member this.Text = match this with | North -> "North" | East -> "East" | South -> "South" | West -> "West"
    member this.ShortText = match this with | North -> "N" | East -> "E" | South -> "S" | West -> "W"

let [<Literal>] CARDS_PER_HAND = 13

let private unshuffledDeck : CardS =
    [ Spade ; Heart ; Diamond ; Club ]
    |> List.collect (fun suit -> [ Ace ; King ; Queen ; Jack ; Ten ; Nine ; Eight ; Seven ; Six ; Five ; Four ; Three ; Two ] |> List.map (fun rank -> rank, suit))
    |> Set.ofList

let cardText (card:Card) = $"{(fst card).ShortText}{(snd card).ShortTextLower}"

let deckText (deck:Deck) = deck |> List.map cardText |> String.concat " "

let cardsText (cards:CardS) = cards |> List.ofSeq |> List.sort |> deckText

let shuffledDeck () : Deck = unshuffledDeck |> List.ofSeq |> List.zip (randoms unshuffledDeck.Count) |> List.sortBy fst |> List.map snd

let hcp (hand:Hand) =
    if hand.Count <> CARDS_PER_HAND then failwith $"{(nameof Hand)} does not contain {CARDS_PER_HAND} {(nameof Card)}s"
    hand |> List.ofSeq |> List.sumBy (fun card -> (fst card).Hcp)
