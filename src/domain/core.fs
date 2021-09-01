module Aornota.BridgeSim.Domain.Core

open Aornota.BridgeSim.Common.Mathy

let [<Literal>] CARDS_PER_HAND = 13

type [<Measure>] hcp // high-card points

type Rank = | Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two
    with
    member this.Hcp = match this with | Ace -> 4<hcp> | King -> 3<hcp> | Queen -> 2<hcp> | Jack -> 1<hcp> | _ -> 0<hcp>
    member this.Text = match this with | Ace -> "Ace" | King -> "King" | Queen -> "Queen" | Jack -> "Jack" | Ten -> "Ten" | Nine -> "Nine" | Eight -> "Eight" | Seven -> "Seven" | Six -> "Six" | Five -> "Five" | Four -> "Four" | Three -> "Three" | Two -> "Two"
    member this.ShortText =
        match this with
        | Ace | King | Queen | Jack | Ten -> this.Text.Substring(0, 1)
        | Nine -> "9" | Eight -> "8" | Seven -> "7" | Six -> "6" | Five -> "5" | Four -> "4" | Three -> "3" | Two -> "2"

type Suit = | Spade | Heart | Diamond | Club
    with
    member this.IsMajor = match this with | Spade | Heart -> true | Diamond | Club -> false
    member this.IsMinor = not this.IsMajor
    member this.Rank = match this with | Spade -> 4 | Heart -> 3 | Diamond -> 2 | Club -> 1
    member this.Text = match this with | Spade -> "Spade" | Heart -> "Heart" | Diamond -> "Diamond" | Club -> "Club"
    member this.TextPlural = $"{this.Text}s"
    member this.ShortText = match this with | Spade -> "S" | Heart -> "H" | Diamond -> "D" | Club -> "C"
    member this.ShortTextLower = this.ShortText.ToLowerInvariant()
    member this.Symbol = match this with | Spade -> "♠" | Heart -> "♥" | Diamond -> "♦" | Club -> "♣"

type Card = private {
    Rank' : Rank
    Suit' : Suit }
    with
    static member Make(rank, suit) = { Rank' = rank ; Suit' = suit }
    member this.ShortText = $"{this.Rank'.ShortText}{this.Suit'.ShortTextLower}"
    member this.SymbolAndRank = $"{this.Suit'.Symbol}{this.Rank'.ShortText}"
    member this.Rank = this.Rank'
    member this.Suit = this.Suit'

type Deck = private { DeckCards' : Card list }
    with
    static member private RequiredUnshuffledCount = CARDS_PER_HAND * 4
    static member private Unshuffled =
        let cards =
            [ Spade ; Heart ; Diamond ; Club ]
            |> List.collect (fun suit -> [ Ace ; King ; Queen ; Jack ; Ten ; Nine ; Eight ; Seven ; Six ; Five ; Four ; Three ; Two ] |> List.map (fun rank -> Card.Make(rank, suit)))
            |> Set.ofList
        if cards.Count <> Deck.RequiredUnshuffledCount then failwith $"SHOULD NEVER HAPPEN -> Unshuffled deck does not contain {Deck.RequiredUnshuffledCount} distinct cards"
        cards
    static member MakeShuffled() = { DeckCards' = Deck.Unshuffled |> List.ofSeq |> List.zip (randoms Deck.RequiredUnshuffledCount) |> List.sortBy fst |> List.map snd }
    member this.Count = this.DeckCards'.Length
    member this.Deal(count) =
        if count > this.Count then failwith $"{nameof(Deck)} does not contain the requested number of cards ({count})"
        this.DeckCards' |> List.take count, { DeckCards' = this.DeckCards' |> List.skip count }
    member this.Text = this.DeckCards' |> List.map (fun card -> card.ShortText) |> String.concat " "

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
    member this.TextUpper = match this with | Balanced -> "Balanced" | SemiBalanced -> "Semi-balanced" | Unbalanced -> "Unbalanced" | VeryUnbalanced -> "Very unbalanced"
    member this.TextLower = $"{this.TextUpper.Substring(0, 1).ToLowerInvariant()}{this.TextUpper.Substring(1)}"

type Hand = private { HandCards' : Set<Card> }
    with
    static member Make(cards) =
        let asSet = cards |> Set.ofList
        if asSet.Count <> CARDS_PER_HAND then failwith $"Hand does not contain {CARDS_PER_HAND} distinct cards"
        { HandCards' = asSet }
    member this.Cards = this.HandCards' |> Seq.sortBy (fun card -> card.Suit, card.Rank) |> List.ofSeq
    member this.Hcp = this.HandCards' |> Seq.sumBy (fun card -> card.Rank.Hcp)
    member this.CardsForSuit(suit) = this.HandCards' |> Seq.filter (fun card -> card.Suit = suit) |> Seq.sortBy (fun card -> card.Rank) |> List.ofSeq
    member this.CountForSuit(suit) = this.HandCards' |> Seq.filter (fun card -> card.Suit = suit) |> Seq.length
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
        | _ -> failwith $"SHOULD NEVER HAPPEN -> {nameof(Hand)} has unexpected shape"
    member this.ShapeCategory = ShapeCategory.Make(this.Shape)
    member this.CardsText(suit) =
        let textForCards = match this.CardsForSuit(suit) with | [] -> "-" | cards -> cards |> List.map (fun card -> card.Rank.ShortText) |> String.concat ""
        $"{suit.Symbol}{textForCards}"

type Position = | North | East | South | West
    with
    member this.LHO = match this with | North -> East | East -> South | South -> West | West -> North
    member this.RHO = match this with | North -> West | East -> North | South -> East | West -> South
    member this.Partner = match this with | North -> South | East -> West | South -> North | West -> East
    member this.IsPartner(position) = this.Partner = position
    member this.IsPartnership(position) = this = position || this.IsPartner(position)
    member this.IsOpponent(position) = not (this.IsPartnership(position))
    member this.Text = match this with | North -> "North" | East -> "East" | South -> "South" | West -> "West"
    member this.ShortText = this.Text.Substring(0, 1)

type Vulnerabilty = | NotVulnerable | Vulnerable
    with
    member this.TextLower = match this with | NotVulnerable -> "not vulnerable" | Vulnerable -> "vulnerable"
    member this.ShortTextLower = match this with | NotVulnerable -> "non-vul." | Vulnerable -> "vul."
