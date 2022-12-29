module Aornota.BridgeSim.Domain.Simulation.HandScenario

open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Evaluation.Core
open Aornota.BridgeSim.Domain.Formatting.Core
open Aornota.BridgeSim.Domain.Simulation.ComparisonConstraint

exception EmptyListForShapeCategoriesException
exception DuplicateShapeCategoriesException of int
exception EmptyListForShapesException
exception DuplicateShapesException of int
exception EmptyListForSuitCountsException
exception DuplicateSuitCountsException of int
exception InvalidSuitCountsException of int * int * int * int
exception SuitConstraintsAllNoneException
exception MinimumSuitLengthsCannotBeSatisfied of int option * int option * int option * int option
exception MaximumSuitLengthsCannotBeSatisfied of int * int * int * int

type ShapeConstraint =
    private
    | ShapeCategories' of ShapeCategory list
    | Shapes' of Shape list
    | SuitCounts' of (int * int * int * int) list
    | SuitConstraints' of SuitConstraint option * SuitConstraint option * SuitConstraint option * SuitConstraint option
    with
    static member ShapeCategories (shapeCategories:ShapeCategory list) =
        match shapeCategories.Length, shapeCategories.Length - (shapeCategories |> Set.ofList).Count with
        | 0, _ -> raise EmptyListForShapeCategoriesException
        | _, 0 -> ShapeCategories' shapeCategories
        | _, duplicateCount -> raise (DuplicateShapeCategoriesException duplicateCount)
    static member Shapes (shapes:Shape list) =
        match shapes.Length, shapes.Length - (shapes |> Set.ofList).Count with
        | 0, _ -> raise EmptyListForShapesException
        | _, 0 -> Shapes' shapes
        | _, duplicateCount -> raise (DuplicateShapesException duplicateCount)
    static member SuitCounts (suitCounts:(int * int * int * int) list) =
        match suitCounts.Length, suitCounts.Length - (suitCounts |> Set.ofList).Count with
        | 0, _ -> raise EmptyListForSuitCountsException
        | _, 0 ->
            suitCounts
            |> List.iter (fun (spadesCount, heartsCount, diamondsCount, clubsCount) ->
                if spadesCount + heartsCount + diamondsCount + clubsCount <> CARDS_PER_HAND then raise (InvalidSuitCountsException (spadesCount, heartsCount, diamondsCount, clubsCount)))
            SuitCounts' suitCounts
        | _, duplicateCount -> raise (DuplicateSuitCountsException duplicateCount)
    static member SuitConstraints (spadesConstraint:SuitConstraint option, heartsConstraint:SuitConstraint option, diamondsConstraint:SuitConstraint option, clubsConstraint:SuitConstraint option) =
        match spadesConstraint, heartsConstraint, diamondsConstraint, clubsConstraint with
        | None, None, None, None -> raise SuitConstraintsAllNoneException
        | _ ->
            let minSpades = match spadesConstraint with | Some spadesConstraint -> spadesConstraint.Min | None -> None
            let minHearts = match heartsConstraint with | Some heartsConstraint -> heartsConstraint.Min | None -> None
            let minDiamonds = match diamondsConstraint with | Some diamondsConstraint -> diamondsConstraint.Min | None -> None
            let minClubs = match clubsConstraint with | Some clubsConstraint -> clubsConstraint.Min | None -> None
            // Always check sigma-min.
            if [ minSpades; minHearts; minDiamonds; minClubs ] |> List.choose id |> List.sum > CARDS_PER_HAND then
                raise (MinimumSuitLengthsCannotBeSatisfied (minSpades, minHearts, minDiamonds, minClubs))
            let maxSpades = match spadesConstraint with | Some spadesConstraint -> spadesConstraint.Max | None -> None
            let maxHearts = match heartsConstraint with | Some heartsConstraint -> heartsConstraint.Max | None -> None
            let maxDiamonds = match diamondsConstraint with | Some diamondsConstraint -> diamondsConstraint.Max | None -> None
            let maxClubs = match clubsConstraint with | Some clubsConstraint -> clubsConstraint.Max | None -> None
            // Only check sigma-max when Some for all Suits.
            match maxSpades, maxHearts, maxDiamonds, maxClubs with
            | Some maxSpades, Some maxHearts, Some maxDiamonds, Some maxClubs when maxSpades + maxHearts + maxDiamonds + maxClubs < CARDS_PER_HAND ->
                raise (MaximumSuitLengthsCannotBeSatisfied (maxSpades, maxHearts, maxDiamonds, maxClubs))
            | _ -> ()
            SuitConstraints' (spadesConstraint, heartsConstraint, diamondsConstraint, clubsConstraint)
    member this.Min (suit:Suit) =
        match this with
        | ShapeCategories' shapeCategories -> shapeCategories |> List.map (fun shapeCategory -> shapeCategory.MinAny) |> List.min
        | Shapes' shapes -> shapes |> List.map (fun shape -> shape.MinAny) |> List.min
        | SuitCounts' suitCounts ->
            suitCounts
            |> List.map (fun (spades, hearts, diamonds, clubs) -> match suit with | Spade -> spades | Heart -> hearts | Diamond -> diamonds | Club -> clubs)
            |> List.min
        | SuitConstraints' (spadesConstraint, heartsConstraint, diamondsConstraint, clubsConstraint) ->
            let maxSpades = spadesConstraint |> Option.map (fun spadesConstraint -> spadesConstraint.Max)
            let maxHearts = heartsConstraint |> Option.map (fun heartsConstraint -> heartsConstraint.Max)
            let maxDiamonds = diamondsConstraint |> Option.map (fun diamondsConstraint -> diamondsConstraint.Max)
            let maxClubs = clubsConstraint |> Option.map (fun clubsConstraint -> clubsConstraint.Max)
            match suit with
            | Spade ->
                match spadesConstraint |> Option.map (fun spadesConstraint -> spadesConstraint.Min) with
                | Some (Some min) -> min
                | _ ->
                    match maxHearts, maxDiamonds, maxClubs with
                    | Some (Some maxHearts), Some (Some maxDiamonds), Some (Some maxClubs) -> CARDS_PER_HAND - (maxHearts + maxDiamonds + maxClubs)
                    | _ -> SuitConstraint.ImplicitMin
            | Heart ->
                match heartsConstraint |> Option.map (fun (heartsConstraint: SuitConstraint) -> heartsConstraint.Min) with
                | Some (Some min) -> min
                | _ ->
                    match maxSpades, maxDiamonds, maxClubs with
                    | Some (Some maxSpades), Some (Some maxDiamonds), Some (Some maxClubs) -> CARDS_PER_HAND - (maxSpades + maxDiamonds + maxClubs)
                    | _ -> SuitConstraint.ImplicitMin
            | Diamond ->
                match diamondsConstraint |> Option.map (fun diamondsConstraint -> diamondsConstraint.Min) with
                | Some (Some min) -> min
                | _ ->
                    match maxSpades, maxHearts, maxClubs with
                    | Some (Some maxSpades), Some (Some maxHearts), Some (Some maxClubs) -> CARDS_PER_HAND - (maxSpades + maxHearts + maxClubs)
                    | _ -> SuitConstraint.ImplicitMin
            | Club ->
                match clubsConstraint |> Option.map (fun clubsConstraint -> clubsConstraint.Min) with
                | Some (Some min) -> min
                | _ ->
                    match maxSpades, maxHearts, maxDiamonds with
                    | Some (Some maxSpades), Some (Some maxHearts), Some (Some maxDiamonds) -> CARDS_PER_HAND - (maxSpades + maxHearts + maxDiamonds)
                    | _ -> SuitConstraint.ImplicitMin
    member this.Max (suit:Suit) =
        match this with
        | ShapeCategories' shapeCategories -> shapeCategories |> List.map (fun shapeCategory -> shapeCategory.MaxAny) |> List.max
        | Shapes' shapes -> shapes |> List.map (fun shape -> shape.MaxAny) |> List.max
        | SuitCounts' suitCounts ->
            suitCounts
            |> List.map (fun (spades, hearts, diamonds, clubs) -> match suit with | Spade -> spades | Heart -> hearts | Diamond -> diamonds | Club -> clubs)
            |> List.max
        | SuitConstraints' (spadesConstraint, heartsConstraint, diamondsConstraint, clubsConstraint) ->
            let minSpades = spadesConstraint |> Option.map (fun spadesConstraint -> spadesConstraint.Min)
            let minHearts = heartsConstraint |> Option.map (fun heartsConstraint -> heartsConstraint.Min)
            let minDiamonds = diamondsConstraint |> Option.map (fun diamondsConstraint -> diamondsConstraint.Min)
            let minClubs = clubsConstraint |> Option.map (fun clubsConstraint -> clubsConstraint.Min)
            match suit with
            | Spade ->
                match spadesConstraint |> Option.map (fun spadesConstraint -> spadesConstraint.Max) with
                | Some (Some max) -> max
                | _ ->
                    match minHearts, minDiamonds, minClubs with
                    | Some (Some minHearts), Some (Some minDiamonds), Some (Some minClubs) -> CARDS_PER_HAND - (minHearts + minDiamonds + minClubs)
                    | _ -> SuitConstraint.ImplicitMax
            | Heart ->
                match heartsConstraint |> Option.map (fun heartsConstraint -> heartsConstraint.Max) with
                | Some (Some max) -> max
                | _ ->
                    match minSpades, minDiamonds, minClubs with
                    | Some (Some minSpades), Some (Some minDiamonds), Some (Some minClubs) -> CARDS_PER_HAND - (minSpades + minDiamonds + minClubs)
                    | _ -> SuitConstraint.ImplicitMax
            | Diamond ->
                match diamondsConstraint |> Option.map (fun diamondsConstraint -> diamondsConstraint.Max) with
                | Some (Some max) -> max
                | _ ->
                    match minSpades, minHearts, minClubs with
                    | Some (Some minSpades), Some (Some minHearts), Some (Some minClubs) -> CARDS_PER_HAND - (minSpades + minHearts + minClubs)
                    | _ -> SuitConstraint.ImplicitMax
            | Club ->
                match clubsConstraint |> Option.map (fun clubsConstraint -> clubsConstraint.Max) with
                | Some (Some max) -> max
                | _ ->
                    match minSpades, minHearts, minDiamonds with
                    | Some (Some minSpades), Some (Some minHearts), Some (Some minDiamonds) -> CARDS_PER_HAND - (minSpades + minHearts + minDiamonds)
                    | _ -> SuitConstraint.ImplicitMax
    member this.Matches (hand:Hand) =
        match this with
        | ShapeCategories' shapeCategories -> shapeCategories |> List.contains hand.ShapeCategory
        | Shapes' shapes -> shapes |> List.contains hand.Shape
        | SuitCounts' suitCounts -> suitCounts |> List.contains hand.SuitCounts
        | SuitConstraints' (spadesConstraint, heartsConstraint, diamondsConstraint, clubsConstraint) ->
            let spadesMatches = match spadesConstraint with | Some spadesConstraint -> spadesConstraint.Matches (hand.CountForSuit Spade) | None -> true
            let heartsMatches = match heartsConstraint with | Some heartsConstraint -> heartsConstraint.Matches (hand.CountForSuit Heart) | None -> true
            let diamondsMatches = match diamondsConstraint with | Some diamondsConstraint -> diamondsConstraint.Matches (hand.CountForSuit Diamond) | None -> true
            let clubsMatches = match clubsConstraint with | Some clubsConstraint -> clubsConstraint.Matches (hand.CountForSuit Club) | None -> true
            spadesMatches && heartsMatches && diamondsMatches && clubsMatches
    member this.Text =
        match this with
        | ShapeCategories' shapeCategories -> shapeCategories |> List.map (fun shapeCategory -> shapeCategory.TextUpper) |> String.concat " or "
        | Shapes' shapes -> shapes |> List.map (fun shape -> $"{shape.Text}") |> String.concat " or "
        | SuitCounts' suitCounts ->
            suitCounts
            |> List.map (fun (spadesCount, heartsCount, diamondsCount, clubsCount) -> $"{spadesCount}={heartsCount}={diamondsCount}={clubsCount}")
            |> String.concat " or "
        | SuitConstraints' (spadesConstraint, heartsConstraint, diamondsConstraint, clubsConstraint) ->
            let spadesText = spadesConstraint |> Option.map (fun spadesConstraint -> $"{spadesConstraint.Text} spades")
            let heartsText = heartsConstraint |> Option.map (fun heartsConstraint -> $"{heartsConstraint.Text} hearts")
            let diamondsText = diamondsConstraint |> Option.map (fun diamondsConstraint -> $"{diamondsConstraint.Text} diamonds")
            let clubsText = clubsConstraint |> Option.map (fun clubsConstraint -> $"{clubsConstraint.Text} clubs")
            match [ spadesText; heartsText; diamondsText; clubsText ] |> List.choose id with
            | h :: t -> h :: t |> String.concat " and "
            | [] -> "(none)" // should never happen

type HandScenario = private {
    Position': Position
    HcpConstraint': HandHcpConstraint option
    CcConstraint': CcConstraint option
    ShapeConstraint': ShapeConstraint option
    Cards': Card list
    CustomPredicate': (Hand -> bool) option }
    with
    static member Make position = {
        Position' = position
        HcpConstraint' = None
        CcConstraint' = None
        ShapeConstraint' = None
        Cards' = []
        CustomPredicate' = None }
    member this.Position = this.Position'
    member this.HcpConstraint = this.HcpConstraint'
    member this.CcConstraint = this.CcConstraint'
    member this.ShapeConstraint = this.ShapeConstraint'
    member this.Cards = this.Cards'
    member this.CustomPredicate = this.CustomPredicate'
    member this.Matches (hand:Hand) =
        let hcpMatches = match this.HcpConstraint with | Some hcpConstraint -> hcpConstraint.Matches hand.Hcp | None -> true
        let ccMatches = match this.CcConstraint with | Some ccConstraint -> ccConstraint.Matches hand.Cc | None -> true
        let shapeMatches = match this.ShapeConstraint with | Some shapeConstraint -> shapeConstraint.Matches hand | None -> true
        let cardsMatches = this.Cards |> List.exists (fun card -> hand.Cards |> List.contains card |> not) |> not
        let customMatches = match this.CustomPredicate with | Some customPredicate -> customPredicate hand | None -> true
        hcpMatches && ccMatches && shapeMatches && cardsMatches && customMatches
    member this.Text =
        let hcpText = this.HcpConstraint |> Option.map (fun hcpConstraint -> hcpConstraint.Text)
        let ccText = this.CcConstraint |> Option.map (fun ccConstraint -> ccConstraint.Text)
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
        $"{this.Position.ShortText}: {constraintsText}"

[<RequireQualifiedAccess>]
type HandProp =
    | Position of Position
    | HcpConstraint of HandHcpConstraint
    | CcConstraint of CcConstraint
    | ShapeConstraint of ShapeConstraint
    | Cards of (Rank * Suit) list
    | CustomPredicate of (Hand -> bool)

exception OtherHandPropIsPositionException of Position
exception HandPropAlreadySpecifiedException of Position * HandProp
exception EmptyListForCardsHandPropException of Position
exception DuplicateCardsForHandPropException of Position * int

let private folder (state:HandScenario) prop =
    match prop with
    | HandProp.Position position -> raise (OtherHandPropIsPositionException position) // should never happen as would not compile
    | HandProp.HcpConstraint _ when state.HcpConstraint |> Option.isSome -> raise (HandPropAlreadySpecifiedException (state.Position, prop))
    | HandProp.CcConstraint _ when state.CcConstraint |> Option.isSome -> raise (HandPropAlreadySpecifiedException (state.Position, prop))
    | HandProp.ShapeConstraint _ when state.ShapeConstraint |> Option.isSome -> raise (HandPropAlreadySpecifiedException (state.Position, prop))
    | HandProp.Cards _ when state.Cards.Length > 0 -> raise (HandPropAlreadySpecifiedException (state.Position, prop))
    | HandProp.CustomPredicate _ when state.CustomPredicate |> Option.isSome -> raise (HandPropAlreadySpecifiedException (state.Position, prop))
    | HandProp.HcpConstraint hcpConstraint -> { state with HcpConstraint' = Some hcpConstraint }
    | HandProp.CcConstraint ccConstraint -> { state with CcConstraint' = Some ccConstraint }
    | HandProp.ShapeConstraint shapeConstraint -> { state with ShapeConstraint' = Some shapeConstraint }
    | HandProp.Cards cards ->
        let cards = cards |> List.map (fun (rank, suit) -> Card.Make (rank, suit))
        match cards.Length, cards.Length - (cards |> Set.ofList).Count with
        | 0, _ -> raise (EmptyListForCardsHandPropException state.Position)
        | _, 0 -> { state with Cards' = cards }
        | _, duplicateCount -> raise (DuplicateCardsForHandPropException (state.Position, duplicateCount))
    | HandProp.CustomPredicate customPredicate -> { state with CustomPredicate' = Some customPredicate }

exception CardsCannotSatisfyMinimumHandHcpException of Position * Card list * int<hcp>
exception CardsCannotSatisfyMinimumHandCcException of Position * Card list * int<cc>
exception CardsCannotSatisfyMaximumHandHcpException of Position * Card list * int<hcp>
exception CardsCannotSatisfyMaximumHandCcException of Position * Card list * int<cc>
exception CardsCannotSatisfyMinimumSuitLengthException of Position * Card list * Suit * int
exception CardsCannotSatisfyMaximumSuitLengthException of Position * Card list * Suit * int

let validate (state:HandScenario) =
    // Only validate minimum HCP / CC / suit lengths if all Cards specified (i.e. optimistically assume constraint could be satisfied by "missing" Cards).
    if state.Cards.Length = CARDS_PER_HAND then
        let hand = Hand.Make state.Cards // can use Hand as have requisite number of Cards
        match state.HcpConstraint |> Option.map (fun hcpConstraint -> hcpConstraint.Min), hand.Hcp with
        | Some min, hcp when hcp < min -> raise (CardsCannotSatisfyMinimumHandHcpException (state.Position, state.Cards, min))
        | _ -> ()
        match state.CcConstraint |> Option.map (fun ccConstraint -> ccConstraint.Min), hand.Cc with
        | Some min, cc when cc < min -> raise (CardsCannotSatisfyMinimumHandCcException (state.Position, state.Cards, min))
        | _ -> ()
        match state.ShapeConstraint with
        | Some shapeConstraint ->
            let spadesCount, heartsCount, diamondsCount, clubsCount = hand.SuitCounts
            let minSpades, minHearts, minDiamonds, minClubs = shapeConstraint.Min Spade, shapeConstraint.Min Heart, shapeConstraint.Min Diamond, shapeConstraint.Min Club
            if spadesCount < minSpades then raise (CardsCannotSatisfyMinimumSuitLengthException (state.Position, state.Cards, Spade, minSpades))
            if heartsCount < minHearts then raise (CardsCannotSatisfyMinimumSuitLengthException (state.Position, state.Cards, Heart, minHearts))
            if diamondsCount < minDiamonds then raise (CardsCannotSatisfyMinimumSuitLengthException (state.Position, state.Cards, Diamond, minDiamonds))
            if clubsCount < minClubs then raise (CardsCannotSatisfyMinimumSuitLengthException (state.Position, state.Cards, Club, minClubs))
        | None -> ()
    // Always validate maximum HCP / CC / suit lengths if any Cards specified.
    if state.Cards.Length > 0 then
        match state.HcpConstraint |> Option.map (fun hcpConstraint -> hcpConstraint.Max), hcp state.Cards with
        | Some max, hcp when hcp > max -> raise (CardsCannotSatisfyMaximumHandHcpException (state.Position, state.Cards, max))
        | _ -> ()
        match state.CcConstraint |> Option.map (fun ccConstraint -> ccConstraint.Max), cc state.Cards with
        | Some max, cc when cc > max -> raise (CardsCannotSatisfyMaximumHandCcException (state.Position, state.Cards, max))
        | _ -> ()
        match state.ShapeConstraint with
        | Some shapeConstraint ->
            let suitCount suit = (cardsForSuit suit state.Cards).Length
            let spadesCount, heartsCount, diamondsCount, clubsCount = suitCount Spade, suitCount Heart, suitCount Diamond, suitCount Club
            let maxSpades, maxHearts, maxDiamonds, maxClubs = shapeConstraint.Max Spade, shapeConstraint.Max Heart, shapeConstraint.Max Diamond, shapeConstraint.Max Club
            if spadesCount > maxSpades then raise (CardsCannotSatisfyMaximumSuitLengthException (state.Position, state.Cards, Spade, maxSpades))
            if heartsCount > maxHearts then raise (CardsCannotSatisfyMaximumSuitLengthException (state.Position, state.Cards, Heart, maxHearts))
            if diamondsCount > maxDiamonds then raise (CardsCannotSatisfyMaximumSuitLengthException (state.Position, state.Cards, Diamond, maxDiamonds))
            if clubsCount > maxClubs then raise (CardsCannotSatisfyMaximumSuitLengthException (state.Position, state.Cards, Club, maxClubs))
        | None -> ()
    state

exception NoHandPropsException
exception NoOtherHandPropsException of Position
exception FirstHandPropNotPositionException of HandProp

type HandScenarioBuilder() =
    member inline _.Yield (()) = ()

    member _.Run (props: HandProp list) =
        match props |> List.rev with
        | [] -> raise NoHandPropsException // should never happen as would not compile
        | HandProp.Position position :: otherProps ->
            match otherProps with
            | [] -> raise (NoOtherHandPropsException position)
            | _ ->
                otherProps
                |> List.fold folder (HandScenario.Make position)
                |> validate
        | firstProp :: _ -> raise (FirstHandPropNotPositionException firstProp) // should never happen as would not compile

    [<CustomOperation("position")>]
    member inline _.Position ((), position) = [ HandProp.Position position ]

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
