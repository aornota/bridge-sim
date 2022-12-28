module Aornota.BridgeSim.Domain.Formatting.Core

open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Evaluation.Core

type Rank with
    member this.Text =
        match this with
        | Ace -> "Ace" | King -> "King" | Queen -> "Queen" | Jack -> "Jack" | Ten -> "Ten" | Nine -> "Nine" | Eight -> "Eight" | Seven -> "Seven" | Six -> "Six" | Five -> "Five" | Four -> "Four"
        | Three -> "Three" | Two -> "Two"
    member this.ShortText =
        match this with
        | Ace | King | Queen | Jack | Ten -> this.Text.Substring(0, 1)
        | Nine -> "9" | Eight -> "8" | Seven -> "7" | Six -> "6" | Five -> "5" | Four -> "4" | Three -> "3" | Two -> "2"

type Suit with
    member this.Text = match this with | Spade -> "Spade" | Heart -> "Heart" | Diamond -> "Diamond" | Club -> "Club"
    member this.TextPlural = $"{this.Text}s"
    member this.TextLowerPlural = this.TextPlural.ToLowerInvariant()
    member this.ShortText = match this with | Spade -> "S" | Heart -> "H" | Diamond -> "D" | Club -> "C"
    member this.ShortTextLower = this.ShortText.ToLowerInvariant()
    member this.Symbol = match this with | Spade -> "♠" | Heart -> "♥" | Diamond -> "♦" | Club -> "♣"

type Card with
    member this.ShortText = $"{this.Rank.ShortText}{this.Suit.ShortTextLower}"
    member this.SymbolAndRank = $"{this.Suit.Symbol}{this.Rank.ShortText}"

type Deck with
    member this.Text = fst (this.Deal(this.Count)) |> List.map (fun card -> card.ShortText) |> String.concat " "

type Shape with
    member this.Text =
        match this with
        | FourThreeThreeThree -> "4333" | FourFourThreeTwo -> "4432" | FourFourFourOne -> "4441"
        | FiveThreeThreeTwo -> "5332" | FiveFourTwoTwo -> "5422" | FiveFourThreeOne -> "5431" | FiveFourFourZero -> "5440" | FiveFiveTwoOne -> "5521" | FiveFiveThreeZero -> "5530"
        | SixThreeTwoTwo -> "6322" | SixThreeThreeOne -> "6331" | SixFourTwoOne -> "6421" | SixFourThreeZero -> "6430" | SixFiveOneOne -> "6511" | SixFiveTwoZero -> "6520" | SixSixOneZero -> "6610"
        | SevenTwoTwoTwo -> "7222" | SevenThreeTwoOne -> "7321" | SevenThreeThreeZero -> "7330" | SevenFourOneOne -> "7411" | SevenFourTwoZero -> "7420" | SevenFiveOneZero -> "7510" | SevenSixZeroZero -> "7600"
        | EightTwoTwoOne -> "8221" | EightThreeOneOne -> "8311" | EightThreeTwoZero -> "8320" | EightFourOneZero -> "8410" | EightFiveZeroZero -> "8500"
        | NineTwoOneOne -> "9211" | NineTwoTwoZero -> "9220" | NineThreeOneZero -> "9310" | NineFourZeroZero -> "9400"
        | TenOneOneOne -> "(10)111" | TenTwoOneZero -> "(10)210" | TenThreeZeroZero -> "(10)300"
        | ElevenOneOneZero -> "(11)110" | ElevenTwoZeroZero -> "(11)200"
        | TwelveOneZeroZero -> "(12)100"
        | ThirteenZeroZeroZero -> "(13)000"

type ShapeCategory with
    member this.TextUpper = match this with | Balanced -> "Balanced" | SemiBalanced -> "Semi-balanced" | Unbalanced -> "Unbalanced" | VeryUnbalanced -> "Very unbalanced"
    member this.TextLower = $"{this.TextUpper.Substring(0, 1).ToLowerInvariant()}{this.TextUpper.Substring(1)}"

let cardsText (suit:Suit) (cards:Card list) =
    let ranksText =
        match cardsForSuit suit cards with
        | [] -> "-"
        | cards -> cards |> List.map (fun card -> card.Rank.ShortText) |> String.concat ""
    $"{suit.Symbol}{ranksText}"

type Hand
    with
    member this.CardsText suit = this.Cards |> cardsText suit
    member this.SpecificShapeText =
        let spadeCount, heartCount, diamondCount, clubCount = this.SuitCounts
        $"{spadeCount}={heartCount}={diamondCount}={clubCount}"
    member this.Text =
        let hcp = this.Hcp
        let hcpText = if hcp < 10<hcp> then $" {hcp}" else $"{hcp}"
        $"{this.CardsText(Spade)} {this.CardsText(Heart)} {this.CardsText(Diamond)} {this.CardsText(Club)} -- {hcpText} HCP | {this.ShapeCategory.TextLower} ({this.SpecificShapeText}) | CC = {this.Cc}"

type Position with
    member this.Text = match this with | North -> "North" | East -> "East" | South -> "South" | West -> "West"
    member this.ShortText = this.Text.Substring(0, 1)

type Partnership with
    member this.Text = match this with | NorthSouth -> $"{North.Text}/{South.Text}" | EastWest -> $"{East.Text}/{West.Text}"
    member this.ShortText = match this with | NorthSouth -> $"{North.ShortText}/{South.ShortText}" | EastWest -> $"{East.ShortText}/{West.ShortText}"

type Vulnerability with
    member this.TextLower = match this with | NotVulnerable -> "not vulnerable" | Vulnerable -> "vulnerable"
    member this.ShortTextLower = match this with | NotVulnerable -> "non-vul." | Vulnerable -> "vul."
