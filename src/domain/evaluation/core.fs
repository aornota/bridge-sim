module Aornota.BridgeSim.Domain.Evaluation.Core

open Aornota.BridgeSim.Domain.Core

exception UnexpectedHandShapeException

let hcp (cards:Card list) = cards  |> Seq.sumBy (fun card -> card.Rank.Hcp)

let cc (cards:Card list) = cards |> Seq.sumBy(fun card -> card.Rank.Cc)

let cardsForSuit suit (cards:Card list) = cards |> Seq.filter (fun card -> card.Suit = suit) |> List.ofSeq

type Hand with
    member this.Hcp = hcp this.Cards
    member this.Cc = cc this.Cards
    member this.CardsForSuit suit = this.Cards |> cardsForSuit suit
    member this.CountForSuit suit = this.CardsForSuit(suit).Length
    member this.SuitCounts = this.CountForSuit(Spade), this.CountForSuit(Heart), this.CountForSuit(Diamond), this.CountForSuit(Club)
    member this.Shape =
        let spadeCount, heartCount, diamondCount, clubCount = this.SuitCounts
        match [ spadeCount ; heartCount ; diamondCount ; clubCount ] |> List.sortDescending with
        // longest suit is four cards
        | [ 4 ; 3 ; 3 ; 3 ] -> FourThreeThreeThree | [ 4 ; 4 ; 3 ; 2 ] -> FourFourThreeTwo | [ 4 ; 4 ; 4 ; 1 ] -> FourFourFourOne
        // longest suit is five cards
        | [ 5 ; 3 ; 3 ; 2 ] -> FiveThreeThreeTwo | [ 5 ; 4 ; 2 ; 2 ] -> FiveFourTwoTwo | [ 5 ; 4 ; 3 ; 1 ] -> FiveFourThreeOne | [ 5 ; 4 ; 4 ; 0 ] -> FiveFourFourZero
        | [ 5 ; 5 ; 2 ; 1 ] -> FiveFiveTwoOne | [ 5 ; 5 ; 3 ; 0 ] -> FiveFiveThreeZero
        // longest suit is six cards
        | [ 6 ; 3 ; 2 ; 2 ] -> SixThreeTwoTwo | [ 6 ; 3 ; 3 ; 1 ] -> SixThreeThreeOne | [ 6 ; 4 ; 2 ; 1 ] -> SixFourTwoOne | [ 6 ; 4 ; 3 ; 0 ] -> SixFourThreeZero
        | [ 6 ; 5 ; 1 ; 1 ] -> SixFiveOneOne | [ 6 ; 5 ; 2 ; 0 ] -> SixFiveTwoZero | [ 6 ; 6 ; 1 ; 0 ] -> SixSixOneZero
        // longest suit is seven cards
        | [ 7 ; 2 ; 2 ; 2 ] -> SevenTwoTwoTwo | [ 7 ; 3 ; 2 ; 1 ] -> SevenThreeTwoOne | [ 7 ; 3 ; 3 ; 0 ] -> SevenThreeThreeZero | [ 7 ; 4 ; 1 ; 1 ] -> SevenFourOneOne
        | [ 7 ; 4 ; 2 ; 0 ] -> SevenFourTwoZero | [ 7 ; 5 ; 1 ; 0 ] -> SevenFiveOneZero | [ 7 ; 6 ; 0 ; 0 ] -> SevenSixZeroZero
        // longest suit is eight cards
        | [ 8 ; 2 ; 2 ; 1 ] -> EightTwoTwoOne | [ 8 ; 3 ; 1 ; 1 ] -> EightThreeOneOne | [ 8 ; 3 ; 2 ; 0 ] -> EightThreeTwoZero | [ 8 ; 4 ; 1 ; 0 ] -> EightFourOneZero
        | [ 8 ; 5 ; 0 ; 0 ] -> EightFiveZeroZero
        // longest suit is nine cards
        | [ 9 ; 2 ; 1 ; 1 ] -> NineTwoOneOne | [ 9 ; 2 ; 2 ; 0 ] -> NineTwoTwoZero | [ 9 ; 3 ; 1 ; 0 ] -> NineThreeOneZero | [ 9 ; 4 ; 0 ; 0 ] -> NineFourZeroZero
        // longest suit is ten cards
        | [ 10 ; 1 ; 1 ; 1 ] -> TenOneOneOne | [ 10 ; 2 ; 1 ; 0 ] -> TenTwoOneZero | [ 10 ; 3 ; 0 ; 0 ] -> TenThreeZeroZero
        // longest suit is eleven cards
        | [ 11 ; 1 ; 1 ; 0 ] -> ElevenOneOneZero | [ 11 ; 2 ; 0 ; 0 ] -> ElevenTwoZeroZero
        // longest suit is twelve cards
        | [ 12 ; 1 ; 0 ; 0 ] -> TwelveOneZeroZero
        // longest suit is thirteen cards
        | [ 13 ; 0 ; 0 ; 0 ] -> ThirteenZeroZeroZero
        | _ -> raise UnexpectedHandShapeException
    member this.ShapeCategory = ShapeCategory.Make(this.Shape)
    member this.SpecificShapeText =
        let spadeCount, heartCount, diamondCount, clubCount = this.SuitCounts
        $"{spadeCount}={heartCount}={diamondCount}={clubCount}"
    member this.Ltc =
        let ltcForSuit suit =
            let ranks = this.CardsForSuit suit |> List.map (fun card -> card.Rank)
            let has rank = ranks |> List.contains rank
            match ranks with
            | [] -> 0
            | [ _ ] -> if has Ace then 0 else 1
            | [ _ ; _ ] ->
                match has Ace, has King with
                | true, true -> 0
                | true, false | false, true -> 1
                | false, false -> 2
            | _ ->
                match has Ace, has King, has Queen with
                | true, true, true -> 0
                | true, true, false | true, false, true | false, true, true -> 1
                | true, false, false | false, true, false | false, false, true -> 2
                | false, false, false -> 3
        [ Spade ; Heart ; Diamond ; Club ] |> List.map ltcForSuit |> List.sum
    member this.AdjustedLtc =
        // Based on https://en.wikipedia.org/wiki/Losing-Trick_Count#Refinements.
        let adjustedLtcForSuit suit =
            let ranks = this.CardsForSuit suit |> List.map (fun card -> card.Rank)
            let has rank = ranks |> List.contains rank
            match ranks with
            | [] -> 0m
            | [ _ ] -> if has Ace then 0m else 1m
            | [ _ ; _ ] ->
                match has Ace, has King, has Queen with
                | true, true, _ -> 0m
                | true, false, true -> 0.5m
                | true, false, false | false, true, true -> 1m
                | false, true, false -> 1.5m
                | _ -> 2m
            | _ ->
                match has Ace, has King, has Queen, has Jack, has Ten with
                | true, true, true, _, _ -> 0m
                | true, true, false, _, _ -> 1m
                | true, false, false, true, true -> 1.25m
                | true, false, true, _, _ -> 1.5m
                | false, true, false, true, true -> 1.75m
                | true, false, false, _, _ | false, true, true, _, _ -> 2m
                | false, true, false, _, _ -> 2.5m
                | false, false, true, _, _ -> 2.75m
                | _ -> 3m
        [ Spade ; Heart ; Diamond ; Club ] |> List.map adjustedLtcForSuit |> List.sum
