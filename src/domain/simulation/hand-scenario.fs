module Aornota.BridgeSim.Domain.Simulation.HandScenario

open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Evaluation.Core
open Aornota.BridgeSim.Domain.Formatting.Core
open Aornota.BridgeSim.Domain.Formatting.Deal
open Aornota.BridgeSim.Domain.Simulation.ComparisonConstraint

type HcpConstraint = ComparisonConstraint<int<hcp>>
type CcConstraint = ComparisonConstraint<int<cc>>
type SuitConstraint = ComparisonConstraint<int>

exception NoMinAnyForShapeConstraint
exception NoMaxAnyForShapeConstraint

type ShapeConstraint =
    | ShapeCategories of ShapeCategory list
    | Shapes of Shape list
    | SuitCounts of (int * int * int * int) list
    | SuitConstraints of SuitConstraint option * SuitConstraint option * SuitConstraint option * SuitConstraint option
    with
    member this.Min (suit:Suit) =
        match this with
        | ShapeCategories shapeCategories -> shapeCategories |> List.map (fun shapeCategory -> shapeCategory.MinAny) |> List.min |> Some
        | Shapes shapes -> shapes |> List.map (fun shape -> shape.MinAny) |> List.min |> Some
        | SuitCounts suitCounts ->
            suitCounts
            |> List.map (fun (spades, hearts, diamonds, clubs) -> match suit with | Spade -> spades | Heart -> hearts | Diamond -> diamonds | Club -> clubs)
            |> List.min
            |> Some
        | SuitConstraints (spadesConstraint, heartsConstraint, diamondsConstraint, clubsConstraint) ->
            let maxSpades = match spadesConstraint with | Some spadesConstraint -> spadesConstraint.Max | None -> None
            let maxHearts = match heartsConstraint with | Some heartsConstraint -> heartsConstraint.Max | None -> None
            let maxDiamonds = match diamondsConstraint with | Some diamondsConstraint -> diamondsConstraint.Max | None -> None
            let maxClubs = match clubsConstraint with | Some clubsConstraint -> clubsConstraint.Max | None -> None
            match suit, spadesConstraint, heartsConstraint, diamondsConstraint, clubsConstraint with
            | Spade, Some spadesConstraint, _, _, _ -> spadesConstraint.Min
            | Spade, None, _, _, _ ->
                match maxHearts, maxDiamonds, maxClubs with
                | Some maxHearts, Some maxDiamonds, Some maxClubs -> Some (CARDS_PER_HAND - (maxHearts + maxDiamonds + maxClubs))
                | _ -> None
            | Heart, _, Some heartsConstraint, _, _ -> heartsConstraint.Min
            | Heart, _, None, _, _ ->
                match maxSpades, maxDiamonds, maxClubs with
                | Some maxSpades, Some maxDiamonds, Some maxClubs -> Some (CARDS_PER_HAND - (maxSpades + maxDiamonds + maxClubs))
                | _ -> None
            | Diamond, _, _, Some diamondsConstraint, _ -> diamondsConstraint.Min
            | Diamond, _, _, None, _ ->
                match maxSpades, maxHearts, maxClubs with
                | Some maxSpades, Some maxHearts, Some maxClubs -> Some (CARDS_PER_HAND - (maxSpades + maxHearts + maxClubs))
                | _ -> None
            | Club, _, _, _, Some clubsConstraint -> clubsConstraint.Min
            | Club, _, _, _, None ->
                match maxSpades, maxHearts, maxDiamonds with
                | Some maxSpades, Some maxHearts, Some maxDiamonds -> Some (CARDS_PER_HAND - (maxSpades + maxHearts + maxDiamonds))
                | _ -> None
    member this.MinAny =
        match [ this.Min Spade; this.Min Heart; this.Min Diamond; this.Min Club ] |> List.choose id with
        | [] -> raise NoMinAnyForShapeConstraint
        | h :: t -> h :: t |> List.min
    member this.Max (suit:Suit) =
        match this with
        | ShapeCategories shapeCategories -> shapeCategories |> List.map (fun shapeCategory -> shapeCategory.MaxAny) |> List.max |> Some
        | Shapes shapes -> shapes |> List.map (fun shape -> shape.MaxAny) |> List.max |> Some
        | SuitCounts suitCounts ->
            suitCounts
            |> List.map (fun (spades, hearts, diamonds, clubs) -> match suit with | Spade -> spades | Heart -> hearts | Diamond -> diamonds | Club -> clubs)
            |> List.max
            |> Some
        | SuitConstraints (spadesConstraint, heartsConstraint, diamondsConstraint, clubsConstraint) ->
            let minSpades = match spadesConstraint with | Some spadesConstraint -> spadesConstraint.Min | None -> None
            let minHearts = match heartsConstraint with | Some heartsConstraint -> heartsConstraint.Min | None -> None
            let minDiamonds = match diamondsConstraint with | Some diamondsConstraint -> diamondsConstraint.Min | None -> None
            let minClubs = match clubsConstraint with | Some clubsConstraint -> clubsConstraint.Min | None -> None
            match suit, spadesConstraint, heartsConstraint, diamondsConstraint, clubsConstraint with
            | Spade, Some spadesConstraint, _, _, _ -> spadesConstraint.Max
            | Spade, None, _, _, _ ->
                match minHearts, minDiamonds, minClubs with
                | Some minHearts, Some minDiamonds, Some minClubs -> Some (CARDS_PER_HAND - (minHearts + minDiamonds + minClubs))
                | _ -> None
            | Heart, _, Some heartsConstraint, _, _ -> heartsConstraint.Max
            | Heart, _, None, _, _ ->
                match minSpades, minDiamonds, minClubs with
                | Some minSpades, Some minDiamonds, Some minClubs -> Some (CARDS_PER_HAND - (minSpades + minDiamonds + minClubs))
                | _ -> None
            | Diamond, _, _, Some diamondsConstraint, _ -> diamondsConstraint.Max
            | Diamond, _, _, None, _ ->
                match minSpades, minHearts, minClubs with
                | Some minSpades, Some minHearts, Some minClubs -> Some (CARDS_PER_HAND - (minSpades + minHearts + minClubs))
                | _ -> None
            | Club, _, _, _, Some clubsConstraint -> clubsConstraint.Max
            | Club, _, _, _, None ->
                match minSpades, minHearts, minDiamonds with
                | Some minSpades, Some minHearts, Some minDiamonds -> Some (CARDS_PER_HAND - (minSpades + minHearts + minDiamonds))
                | _ -> None
    member this.MaxAny =
        match [ this.Max Spade; this.Max Heart; this.Max Diamond; this.Max Club ] |> List.choose id with
        | [] -> raise NoMaxAnyForShapeConstraint
        | h :: t -> h :: t |> List.max
    member this.Matches (hand:Hand) =
        match this with
        | ShapeCategories shapeCategories -> shapeCategories |> List.contains hand.ShapeCategory
        | Shapes shapes -> shapes |> List.contains hand.Shape
        | SuitCounts suitCounts -> suitCounts |> List.contains hand.SuitCounts
        | SuitConstraints (spadesConstraint, heartsConstraint, diamondsConstraint, clubsConstraint) ->
            let spadesMatches = match spadesConstraint with | Some spadesConstraint -> spadesConstraint.Matches (hand.CountForSuit Spade) | None -> true
            let heartsMatches = match heartsConstraint with | Some heartsConstraint -> heartsConstraint.Matches (hand.CountForSuit Heart) | None -> true
            let diamondsMatches = match diamondsConstraint with | Some diamondsConstraint -> diamondsConstraint.Matches (hand.CountForSuit Diamond) | None -> true
            let clubsMatches = match clubsConstraint with | Some clubsConstraint -> clubsConstraint.Matches (hand.CountForSuit Club) | None -> true
            spadesMatches && heartsMatches && diamondsMatches && clubsMatches
    member this.Text =
        match this with
        | ShapeCategories shapeCategories -> "TODO-NMB..."
        | Shapes shapes -> "TODO-NMB..."
        | SuitCounts suitCounts -> "TODO-NMB..."
        | SuitConstraints (spadesConstraint, heartsConstraint, diamondsConstraint, clubsConstraint) ->
            "TODO-NMB..."

type HandScenario = private {
    Seat: Seat
    HcpConstraint: HcpConstraint option
    CcConstraint: CcConstraint option
    ShapeConstraint: ShapeConstraint option
    Cards: Card list
    CustomPredicate: (Hand -> bool) option }
    with
    member this.Matches (hand:Hand) =
        let hcpMatches = match this.HcpConstraint with | Some hcpConstraint -> hcpConstraint.Matches hand.Hcp | None -> true
        let ccMatches = match this.CcConstraint with | Some ccConstraint -> ccConstraint.Matches hand.Cc | None -> true
        let shapeMatches = match this.ShapeConstraint with | Some shapeConstraint -> shapeConstraint.Matches hand | None -> true
        let cardsMatches = this.Cards |> List.exists (fun card -> hand.Cards |> List.contains card |> not) |> not
        let customMatches = match this.CustomPredicate with | Some customPredicate -> customPredicate hand | None -> true
        hcpMatches && ccMatches && shapeMatches && cardsMatches && customMatches
    member this.Text =
        let hcpText = this.HcpConstraint |> Option.map (fun hcpConstraint -> $"{hcpConstraint.Text} HCP")
        let ccText = this.CcConstraint |> Option.map (fun ccConstraint -> $"{ccConstraint.Text} CC")
        let shapeText = this.ShapeConstraint |> Option.map (fun shapeConstraint -> shapeConstraint.Text)
        let cardsText =
            match this.Cards with
            | [] -> None
            | cards ->
                let preamble = if cards.Length = CARDS_PER_HAND then "exactly" else "containing"
                Some $"{preamble} {cards |> cardsText Spade} {cards |> cardsText Heart} {cards |> cardsText Diamond} {cards |> cardsText Club}"
        let customPredicateText = this.CustomPredicate |> Option.map (fun _ -> "with custom predicate")
        let constraintsText =
            match [ hcpText; ccText; shapeText; cardsText; customPredicateText ] |> List.choose id with
            | h :: t -> h :: t |> String.concat " | "
            | [] -> "(none)" // should never happen
        $"{this.Seat.ShortText}: {constraintsText}"

[<RequireQualifiedAccess>]
type HandProp =
    | Seat of Seat
    | HcpConstraint of HcpConstraint
    | CcConstraint of CcConstraint
    | ShapeConstraint of ShapeConstraint
    | Cards of (Rank * Suit) list
    | CustomPredicate of (Hand -> bool)

let [<Literal>] private MIN_HAND_HCP = 0<hcp>
let [<Literal>] private MAX_HAND_HCP = 37<hcp>
let [<Literal>] private MIN_HAND_CC = 0<cc>
let [<Literal>] private MAX_HAND_CC = 12<cc>
let [<Literal>] private MIN_HAND_SUIT = 0
let [<Literal>] private MAX_HAND_SUIT = CARDS_PER_HAND

let private initialState seat = {
    Seat = seat
    HcpConstraint = None
    CcConstraint = None
    ShapeConstraint = None
    Cards = []
    CustomPredicate = None }

exception OtherHandPropIsSeatException of Seat
exception HandPropAlreadySpecifiedException of Seat * HandProp
exception MinHandHcpInvalidException of Seat * ComparisonConstraint<int<hcp>> * int<hcp>
exception MaxHandHcpInvalidException of Seat * ComparisonConstraint<int<hcp>> * int<hcp>
exception BetweenHandHcpInvalidException of Seat * ComparisonConstraint<int<hcp>>
exception MinHandCcInvalidException of Seat * ComparisonConstraint<int<cc>> * int<cc>
exception MaxHandCcInvalidException of Seat * ComparisonConstraint<int<cc>> * int<cc>
exception BetweenHandCcInvalidException of Seat * ComparisonConstraint<int<cc>>
exception EmptyListForShapeHandPropException of Seat * ShapeConstraint
exception AllNoneForShapeHandPropException of Seat * ShapeConstraint
exception MinHandSuitInvalidException of Seat * ShapeConstraint * int
exception MaxHandSuitInvalidException of Seat * ShapeConstraint * int
exception BetweenHandSuitInvalidException of Seat * ShapeConstraint
exception EmptyListForCardsHandPropException of Seat
exception DuplicateCardsForHandPropException of Seat * int

let private folder state prop =
    match prop with
    | HandProp.Seat seat -> raise (OtherHandPropIsSeatException seat) // should never happen as would not compile
    | HandProp.HcpConstraint _ when state.HcpConstraint |> Option.isSome -> raise (HandPropAlreadySpecifiedException (state.Seat, prop))
    | HandProp.CcConstraint _ when state.CcConstraint |> Option.isSome -> raise (HandPropAlreadySpecifiedException (state.Seat, prop))
    | HandProp.ShapeConstraint _ when state.ShapeConstraint |> Option.isSome -> raise (HandPropAlreadySpecifiedException (state.Seat, prop))
    | HandProp.Cards _ when state.Cards.Length > 0 -> raise (HandPropAlreadySpecifiedException (state.Seat, prop))
    | HandProp.CustomPredicate _ when state.CustomPredicate |> Option.isSome -> raise (HandPropAlreadySpecifiedException (state.Seat, prop))
    | HandProp.HcpConstraint hcpConstraint ->
        match hcpConstraint.Min, hcpConstraint.Max with
        | Some min, _ when min < MIN_HAND_HCP -> raise (MinHandHcpInvalidException (state.Seat, hcpConstraint, MIN_HAND_HCP))
        | Some min, _ when min > MAX_HAND_HCP -> raise (MinHandHcpInvalidException (state.Seat, hcpConstraint, MAX_HAND_HCP))
        | _, Some max when max < MIN_HAND_HCP -> raise (MaxHandHcpInvalidException (state.Seat, hcpConstraint, MIN_HAND_HCP))
        | _, Some max when max > MAX_HAND_HCP -> raise (MaxHandHcpInvalidException (state.Seat, hcpConstraint, MAX_HAND_HCP))
        | _ ->
            match hcpConstraint with
            | Between (min, max) when max <= min -> raise (BetweenHandHcpInvalidException (state.Seat, hcpConstraint))
            | _ -> { state with HcpConstraint = Some hcpConstraint }
    | HandProp.CcConstraint ccConstraint ->
        match ccConstraint.Min, ccConstraint.Max with
        | Some min, _ when min < MIN_HAND_CC -> raise (MinHandCcInvalidException (state.Seat, ccConstraint, MIN_HAND_CC))
        | Some min, _ when min > MAX_HAND_CC -> raise (MinHandCcInvalidException (state.Seat, ccConstraint, MAX_HAND_CC))
        | _, Some max when max < MIN_HAND_CC -> raise (MaxHandCcInvalidException (state.Seat, ccConstraint, MIN_HAND_CC))
        | _, Some max when max > MAX_HAND_CC -> raise (MaxHandCcInvalidException (state.Seat, ccConstraint, MAX_HAND_CC))
        | _ ->
            match ccConstraint with
            | Between (min, max) when max <= min -> raise (BetweenHandCcInvalidException (state.Seat, ccConstraint))
            | _ -> { state with CcConstraint = Some ccConstraint }
    | HandProp.ShapeConstraint shapeConstraint ->
        match shapeConstraint, shapeConstraint.MinAny, shapeConstraint.MaxAny with
        | ShapeCategories shapeCategories, _, _ when shapeCategories.Length = 0 -> raise (EmptyListForShapeHandPropException (state.Seat, shapeConstraint))
        | Shapes shapes, _ , _ when shapes.Length = 0 -> raise (EmptyListForShapeHandPropException (state.Seat, shapeConstraint))
        // TODO-NMB: Check all suitCounts sum to CARDS_PER_HAND...
        | SuitCounts suitCounts, _, _ when suitCounts.Length = 0 -> raise (EmptyListForShapeHandPropException (state.Seat, shapeConstraint))
        // TODO-NMB: Check (perhaps below) that sum of min-suit <= CARDS_PER_HAND anf sum of max-suit >= CARDS_PER_HAND...
        | SuitConstraints (None, None, None, None), _ , _ -> raise (AllNoneForShapeHandPropException (state.Seat, shapeConstraint))
        | _, minAny, _ when minAny < MIN_HAND_SUIT -> raise (MinHandSuitInvalidException (state.Seat, shapeConstraint, MIN_HAND_SUIT))
        | _, minAny, _ when minAny > MAX_HAND_SUIT -> raise (MinHandSuitInvalidException (state.Seat, shapeConstraint, MAX_HAND_SUIT))
        | _, _, maxAny when maxAny < MIN_HAND_SUIT -> raise (MaxHandSuitInvalidException (state.Seat, shapeConstraint, MIN_HAND_SUIT))
        | _, _, maxAny when maxAny > MAX_HAND_SUIT -> raise (MaxHandSuitInvalidException (state.Seat, shapeConstraint, MAX_HAND_SUIT))
        | _ ->
            match shapeConstraint with
            | SuitConstraints (spadesConstraint, heartsConstraint, diamondsConstraint, clubsConstraint) ->
                match spadesConstraint, heartsConstraint, diamondsConstraint, clubsConstraint with
                | Some (Between (min, max)), _, _, _ when max <= min -> raise (BetweenHandSuitInvalidException (state.Seat, shapeConstraint))
                | _, Some (Between (min, max)), _, _ when max <= min -> raise (BetweenHandSuitInvalidException (state.Seat, shapeConstraint))
                | _, _, Some (Between (min, max)), _ when max <= min -> raise (BetweenHandSuitInvalidException (state.Seat, shapeConstraint))
                | _, _, _, Some (Between (min, max)) when max <= min -> raise (BetweenHandSuitInvalidException (state.Seat, shapeConstraint))
                | _ -> { state with ShapeConstraint = Some shapeConstraint }
            | _ -> { state with ShapeConstraint = Some shapeConstraint }
    | HandProp.Cards cards ->
        let cards = cards |> List.map (fun (rank, suit) -> Card.Make (rank, suit))
        match cards.Length, cards.Length - (cards |> Set.ofList).Count with
        | 0, _ -> raise (EmptyListForCardsHandPropException state.Seat)
        | _, 0 -> { state with Cards = cards }
        | _, duplicateCount -> raise (DuplicateCardsForHandPropException (state.Seat, duplicateCount))
    | HandProp.CustomPredicate customPredicate -> { state with CustomPredicate = Some customPredicate }

exception CardsCannotSatisfyMinHandHcpException of Seat * Card list * int<hcp>
exception CardsCannotSatisfyMinHandCcException of Seat * Card list * int<cc>
exception CardsCannotSatisfyMaxHandHcpException of Seat * Card list * int<hcp>
exception CardsCannotSatisfyMaxHandCcException of Seat * Card list * int<cc>

let validate state =
    if state.Cards.Length = CARDS_PER_HAND then // only validate minimum HCP / CC if all Cards specified (i.e. optimistically assume constraint could be satisfied by "missing" Cards)
        let hand = Hand.Make state.Cards // can use Hand as have requisite number of Cards
        match state.HcpConstraint with
        | Some hcpConstraint ->
            match hcpConstraint.Min, hand.Hcp with
            | Some min, hcp when hcp < min -> raise (CardsCannotSatisfyMinHandHcpException (state.Seat, state.Cards, min))
            | _ -> ()
        | None -> ()
        match state.CcConstraint with
        | Some ccConstraint ->
            match ccConstraint.Min, hand.Cc with
            | Some min, cc when cc < min -> raise (CardsCannotSatisfyMinHandCcException (state.Seat, state.Cards, min))
            | _ -> ()
        | None -> ()
    if state.Cards.Length > 0 then // only validate maximum HCP / CC if some Cards specified.
        match state.HcpConstraint with
        | Some hcpConstraint ->
            match hcpConstraint.Max, hcp state.Cards with
            | Some max, hcp when hcp > max -> raise (CardsCannotSatisfyMaxHandHcpException (state.Seat, state.Cards, max))
            | _ -> ()
        | None -> ()
        match state.CcConstraint with
        | Some ccConstraint ->
            match ccConstraint.Max, cc state.Cards with
            | Some max, cc when cc > max -> raise (CardsCannotSatisfyMaxHandCcException (state.Seat, state.Cards, max))
            | _ -> ()
        | None -> ()
    // TODO-NMB: More validation (e.g. shape constraints &c.)...
    state

exception NoHandPropsException
exception NoOtherHandPropsException of Seat
exception FirstHandPropNotSeatException of HandProp

type HandScenarioBuilder() =
    member inline _.Yield (()) = ()

    member _.Run (props: HandProp list) =
        match props |> List.rev with
        | [] -> raise NoHandPropsException // should never happen as would not compile
        | HandProp.Seat seat :: otherProps ->
            match otherProps with
            | [] -> raise (NoOtherHandPropsException seat)
            | _ ->
                otherProps
                |> List.fold folder (initialState seat)
                |> validate
        | firstProp :: _ -> raise (FirstHandPropNotSeatException firstProp) // should never happen as would not compile

    [<CustomOperation("seat")>]
    member inline _.Seat ((), seat) = [ HandProp.Seat seat ]

    [<CustomOperation("hcp")>]
    member inline _.HcpConstraint (props, hcpConstraint) = HandProp.HcpConstraint hcpConstraint :: props

    [<CustomOperation("cc")>]
    member inline _.CcConstraint (props, ccConstraint) = HandProp.CcConstraint ccConstraint :: props

    [<CustomOperation("shape")>]
    member inline _.ShapeConstraint (props, shapeConstraint) = HandProp.ShapeConstraint shapeConstraint :: props

    [<CustomOperation("cards")>]
    member inline _.Cards (props, cards) = HandProp.Cards cards :: props

    [<CustomOperation("customPredicate")>]
    member inline _.CustomPredicate (props, customPredicate) = HandProp.CustomPredicate customPredicate :: props

let handScenario = HandScenarioBuilder ()
