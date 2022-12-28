module Aornota.BridgeSim.Domain.Simulation.HandScenario

open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Evaluation.Core
open Aornota.BridgeSim.Domain.Formatting.Core
open Aornota.BridgeSim.Domain.Formatting.Deal
open Aornota.BridgeSim.Domain.Simulation.Constraint

// TODO-NMB: Shape constraints | ...

type HandScenario = {
    Seat: Seat
    HcpConstraint: ComparisonConstraint<int<hcp>> option
    CcConstraint: ComparisonConstraint<int<cc>> option
    Cards: Card list
    CustomPredicate: (Hand -> bool) option }
    with
    member this.Text =
        let hcpText = this.HcpConstraint |> Option.map (fun hcpConstraint -> $"{hcpConstraint.Text} HCP")
        let ccText = this.CcConstraint |> Option.map (fun ccConstraint -> $"{ccConstraint.Text} CC")
        let cardsText =
            match this.Cards with
            | [] -> None
            | cards -> Some $"containing {cards |> cardsForSuit Spade} {cards |> cardsForSuit Heart} {cards |> cardsForSuit Diamond} {cards |> cardsForSuit Club}"
        let customPredicateText = this.CustomPredicate |> Option.map (fun _ -> "with custom predicate")
        let constraintsText =
            match [ hcpText; ccText; cardsText; customPredicateText ] |> List.choose id with
            | h :: t -> h :: t |> String.concat " | "
            | [] -> "(none)"
        $"{this.Seat.ShortText}: {constraintsText}"

[<RequireQualifiedAccess>]
type HandProp =
    | Seat of Seat
    | HcpConstraint of ComparisonConstraint<int<hcp>>
    | CcConstraint of ComparisonConstraint<int<cc>>
    | Cards of (Rank * Suit) list
    | CustomPredicate of (Hand -> bool)

exception NoHandPropsException
exception NoOtherHandPropsException of Seat
exception FirstHandPropNotSeatException of HandProp

exception OtherHandPropIsSeatException of Seat
exception HandPropAlreadySpecifiedException of Seat * HandProp
exception MinHandHcpInvalidException of Seat * ComparisonConstraint<int<hcp>> * int<hcp>
exception MaxHandHcpInvalidException of Seat * ComparisonConstraint<int<hcp>> * int<hcp>
exception BetweenHandHcpInvalidException of Seat * ComparisonConstraint<int<hcp>>
exception MinHandCcInvalidException of Seat * ComparisonConstraint<int<cc>> * int<cc>
exception MaxHandCcInvalidException of Seat * ComparisonConstraint<int<cc>> * int<cc>
exception BetweenHandCcInvalidException of Seat * ComparisonConstraint<int<cc>>
exception DuplicateCardsForHandPropException of Seat * int

exception CardsCannotSatisfyMinHandHcpException of Seat * Card list * int<hcp>
exception CardsCannotSatisfyMinHandCcException of Seat * Card list * int<cc>
exception CardsCannotSatisfyMaxHandHcpException of Seat * Card list * int<hcp>
exception CardsCannotSatisfyMaxHandCcException of Seat * Card list * int<cc>

let [<Literal>] private MIN_HAND_HCP = 0<hcp>
let [<Literal>] private MAX_HAND_HCP = 27<hcp>
let [<Literal>] private MIN_HAND_CC = 0<cc>
let [<Literal>] private MAX_HAND_CC = 12<cc>

let inline private initialState seat = {
    Seat = seat
    HcpConstraint = None
    CcConstraint = None
    Cards = []
    CustomPredicate = None }

let inline private folder state prop =
    match prop with
    | HandProp.Seat seat -> raise (OtherHandPropIsSeatException seat) // should never happen as would not compile
    | HandProp.HcpConstraint _ when state.HcpConstraint |> Option.isSome -> raise (HandPropAlreadySpecifiedException (state.Seat, prop))
    | HandProp.CcConstraint _ when state.CcConstraint |> Option.isSome -> raise (HandPropAlreadySpecifiedException (state.Seat, prop))
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
    | HandProp.Cards cards ->
        let cards = cards |> List.map (fun (rank, suit) -> Card.Make (rank, suit))
        match cards.Length - (cards |> Set.ofList).Count with
        | 0 -> { state with Cards = cards }
        | duplicateCount -> raise (DuplicateCardsForHandPropException (state.Seat, duplicateCount))
    | HandProp.CustomPredicate customPredicate -> { state with CustomPredicate = Some customPredicate }

let inline validate state =
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

type HandScenarioBuilder() =
    member inline _.Yield (()) = ()

    member inline _.Run (props: HandProp list) =
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

    [<CustomOperation("cards")>]
    member inline _.Cards (props, cards) = HandProp.Cards cards :: props

    [<CustomOperation("customPredicate")>]
    member inline _.CustomPredicate (props, customPredicate) = HandProp.CustomPredicate customPredicate :: props

let handScenario = HandScenarioBuilder ()
