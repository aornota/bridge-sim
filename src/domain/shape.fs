module Aornota.BridgeSim.Domain.Shape

open Aornota.BridgeSim.Domain.Core

type Shape =
    | FourThreeThreeThree | FourFourThreeTwo | FourFourFourOne // longest suit is four cards
    | FiveThreeThreeTwo | FiveFourTwoTwo | FiveFourThreeOne | FiveFourFourZero | FiveFiveTwoOne | FiveFiveThreeZero // longest suit is five cards
    | SixThreeTwoTwo | SixThreeThreeOne | SixFourTwoOne | SixFourThreeZero | SixFiveOneOne | SixFiveTwoZero | SixSixOneZero // longest suit is six cards
    | SevenTwoTwoTwo | SevenThreeTwoOne | SevenThreeThreeZero | SevenFourOneOne | SevenFourTwoZero | SevenFiveOneZero | SevenSixZeroZero // longest suit is seven cards
    | EightTwoTwoOne | EightThreeOneOne | EightThreeTwoZero | EightFourOneZero | EightFiveZeroZero // longest suit is eight cards
    | NineTwoOneOne | NineTwoTwoZero | NineThreeOneZero | NineFourZeroZero // longest suit is nine cards
    | TenOneOneOne | TenTwoOneZero | TenThreeZeroZero // longest suit is ten cards
    | ElevenOneOneZero | ElevenTwoZeroZero // longest suit is eleven cards
    | TwelveOneZeroZero // longest suit is twelve cards
    | ThirteenZeroZeroZero // longest suit is thirteen cards
    with
    static member Make(hand:Hand) =
        if hand.Count <> CARDS_PER_HAND then failwith $"{(nameof Hand)} does not contain {CARDS_PER_HAND} {(nameof Card)}s"
        let count suit = hand |> List.ofSeq |> List.filter (fun card -> snd card = suit) |> List.length
        match [ count Spade ; count Heart ; count Diamond ; count Club ] |> List.sortDescending with
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
        | _ -> failwith $"Unexpected shape for {(nameof Hand)} ({hand})"

type ShapeCategory =
    | Balanced // no singletons or voids and at most one doubleton
    | SemiBalanced // no singletons or voids and exactly two doubletons
    | Unbalanced // at least one singleton or void (or three doubletons) and exactly nine cards between two longest suits
    | VeryUnbalanced // at least one singleton or void and ten or more cards between two longest suits
    with
    static member Make = function
        | FourThreeThreeThree | FourFourThreeTwo | FiveThreeThreeTwo -> Balanced
        | FiveFourTwoTwo | SixThreeTwoTwo -> SemiBalanced
        | FourFourFourOne | FiveFourThreeOne | FiveFourFourZero | SixThreeThreeOne | SevenTwoTwoTwo -> Unbalanced
        | _ -> VeryUnbalanced

