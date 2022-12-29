module Aornota.BridgeSim.Domain.Simulation.PartnershipScenario

open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Evaluation.Core
open Aornota.BridgeSim.Domain.Formatting.Core
open Aornota.BridgeSim.Domain.Simulation.ComparisonConstraint

let private CARDS_PER_PARTNERSHIP = CARDS_PER_HAND * 2

exception EmptyListForPartnershipSuitCountsException
exception DuplicatePartnershipSuitCountsException of int
exception InvalidPartnershipSuitCountsException of int * int * int * int
exception PartnershipSuitConstraintsAllNoneException
exception MinimumPartnershipSuitLengthsCannotBeSatisfied of int option * int option * int option * int option
exception MaximumPartnershipSuitLengthsCannotBeSatisfied of int * int * int * int

type PartnershipShapeConstraint =
    private
    // No ShapeCategory' or Shape' (cf. HandScenario.ShapeConstraint).
    | SuitCounts' of (int * int * int * int) list
    | SuitConstraints' of SuitConstraint option * SuitConstraint option * SuitConstraint option * SuitConstraint option
    static member SuitCounts (suitCounts:(int * int * int * int) list) =
        match suitCounts.Length, suitCounts.Length - (suitCounts |> Set.ofList).Count with
        | 0, _ -> raise EmptyListForPartnershipSuitCountsException
        | _, 0 ->
            suitCounts
            |> List.iter (fun (spadesCount, heartsCount, diamondsCount, clubsCount) ->
                if spadesCount + heartsCount + diamondsCount + clubsCount <> CARDS_PER_PARTNERSHIP then raise (InvalidPartnershipSuitCountsException (spadesCount, heartsCount, diamondsCount, clubsCount)))
            SuitCounts' suitCounts
        | _, duplicateCount -> raise (DuplicatePartnershipSuitCountsException duplicateCount)
    static member SuitConstraints (spadesConstraint:SuitConstraint option, heartsConstraint:SuitConstraint option, diamondsConstraint:SuitConstraint option, clubsConstraint:SuitConstraint option) =
        match spadesConstraint, heartsConstraint, diamondsConstraint, clubsConstraint with
        | None, None, None, None -> raise PartnershipSuitConstraintsAllNoneException
        | _ ->
            let minSpades = match spadesConstraint with | Some spadesConstraint -> spadesConstraint.Min | None -> None
            let minHearts = match heartsConstraint with | Some heartsConstraint -> heartsConstraint.Min | None -> None
            let minDiamonds = match diamondsConstraint with | Some diamondsConstraint -> diamondsConstraint.Min | None -> None
            let minClubs = match clubsConstraint with | Some clubsConstraint -> clubsConstraint.Min | None -> None
            // Always check sigma-min.
            if [ minSpades; minHearts; minDiamonds; minClubs ] |> List.choose id |> List.sum > CARDS_PER_PARTNERSHIP then
                raise (MinimumPartnershipSuitLengthsCannotBeSatisfied (minSpades, minHearts, minDiamonds, minClubs))
            let maxSpades = match spadesConstraint with | Some spadesConstraint -> spadesConstraint.Max | None -> None
            let maxHearts = match heartsConstraint with | Some heartsConstraint -> heartsConstraint.Max | None -> None
            let maxDiamonds = match diamondsConstraint with | Some diamondsConstraint -> diamondsConstraint.Max | None -> None
            let maxClubs = match clubsConstraint with | Some clubsConstraint -> clubsConstraint.Max | None -> None
            // Only check sigma-max when Some for all Suits.
            match maxSpades, maxHearts, maxDiamonds, maxClubs with
            | Some maxSpades, Some maxHearts, Some maxDiamonds, Some maxClubs when maxSpades + maxHearts + maxDiamonds + maxClubs < CARDS_PER_PARTNERSHIP ->
                raise (MaximumPartnershipSuitLengthsCannotBeSatisfied (maxSpades, maxHearts, maxDiamonds, maxClubs))
            | _ -> ()
            SuitConstraints' (spadesConstraint, heartsConstraint, diamondsConstraint, clubsConstraint)
    member this.Min (suit:Suit) =
        match this with
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
                    | Some (Some maxHearts), Some (Some maxDiamonds), Some (Some maxClubs) -> CARDS_PER_PARTNERSHIP - (maxHearts + maxDiamonds + maxClubs)
                    | _ -> SuitConstraint.ImplicitMin
            | Heart ->
                match heartsConstraint |> Option.map (fun (heartsConstraint: SuitConstraint) -> heartsConstraint.Min) with
                | Some (Some min) -> min
                | _ ->
                    match maxSpades, maxDiamonds, maxClubs with
                    | Some (Some maxSpades), Some (Some maxDiamonds), Some (Some maxClubs) -> CARDS_PER_PARTNERSHIP - (maxSpades + maxDiamonds + maxClubs)
                    | _ -> SuitConstraint.ImplicitMin
            | Diamond ->
                match diamondsConstraint |> Option.map (fun diamondsConstraint -> diamondsConstraint.Min) with
                | Some (Some min) -> min
                | _ ->
                    match maxSpades, maxHearts, maxClubs with
                    | Some (Some maxSpades), Some (Some maxHearts), Some (Some maxClubs) -> CARDS_PER_PARTNERSHIP - (maxSpades + maxHearts + maxClubs)
                    | _ -> SuitConstraint.ImplicitMin
            | Club ->
                match clubsConstraint |> Option.map (fun clubsConstraint -> clubsConstraint.Min) with
                | Some (Some min) -> min
                | _ ->
                    match maxSpades, maxHearts, maxDiamonds with
                    | Some (Some maxSpades), Some (Some maxHearts), Some (Some maxDiamonds) -> CARDS_PER_PARTNERSHIP - (maxSpades + maxHearts + maxDiamonds)
                    | _ -> SuitConstraint.ImplicitMin
    member this.Max (suit:Suit) =
        match this with
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
                    | Some (Some minHearts), Some (Some minDiamonds), Some (Some minClubs) -> CARDS_PER_PARTNERSHIP - (minHearts + minDiamonds + minClubs)
                    | _ -> SuitConstraint.ImplicitMax
            | Heart ->
                match heartsConstraint |> Option.map (fun heartsConstraint -> heartsConstraint.Max) with
                | Some (Some max) -> max
                | _ ->
                    match minSpades, minDiamonds, minClubs with
                    | Some (Some minSpades), Some (Some minDiamonds), Some (Some minClubs) -> CARDS_PER_PARTNERSHIP - (minSpades + minDiamonds + minClubs)
                    | _ -> SuitConstraint.ImplicitMax
            | Diamond ->
                match diamondsConstraint |> Option.map (fun diamondsConstraint -> diamondsConstraint.Max) with
                | Some (Some max) -> max
                | _ ->
                    match minSpades, minHearts, minClubs with
                    | Some (Some minSpades), Some (Some minHearts), Some (Some minClubs) -> CARDS_PER_PARTNERSHIP - (minSpades + minHearts + minClubs)
                    | _ -> SuitConstraint.ImplicitMax
            | Club ->
                match clubsConstraint |> Option.map (fun clubsConstraint -> clubsConstraint.Max) with
                | Some (Some max) -> max
                | _ ->
                    match minSpades, minHearts, minDiamonds with
                    | Some (Some minSpades), Some (Some minHearts), Some (Some minDiamonds) -> CARDS_PER_PARTNERSHIP - (minSpades + minHearts + minDiamonds)
                    | _ -> SuitConstraint.ImplicitMax
    member this.Matches (hand1:Hand, hand2:Hand) =
        let spadesCount1, heartsCount1, diamondsCount1, clubsCount1 = hand1.SuitCounts
        let spadesCount2, heartsCount2, diamondsCount2, clubsCount2 = hand1.SuitCounts
        let spadesCount, heartsCount, diamondsCount, clubsCount = spadesCount1 + spadesCount2, heartsCount1 + heartsCount2, diamondsCount1 + diamondsCount2, clubsCount1 + clubsCount2
        match this with
        | SuitCounts' suitCounts -> suitCounts |> List.contains (spadesCount, heartsCount, diamondsCount, clubsCount)
        | SuitConstraints' (spadesConstraint, heartsConstraint, diamondsConstraint, clubsConstraint) ->
            let spadesMatches = match spadesConstraint with | Some spadesConstraint -> spadesConstraint.Matches spadesCount | None -> true
            let heartsMatches = match heartsConstraint with | Some heartsConstraint -> heartsConstraint.Matches heartsCount | None -> true
            let diamondsMatches = match diamondsConstraint with | Some diamondsConstraint -> diamondsConstraint.Matches diamondsCount | None -> true
            let clubsMatches = match clubsConstraint with | Some clubsConstraint -> clubsConstraint.Matches clubsCount | None -> true
            spadesMatches && heartsMatches && diamondsMatches && clubsMatches
    member this.Text =
        match this with
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

type PartnershipScenario = private {
    Partnership': Partnership
    HcpConstraint': PartnershipHcpConstraint option
    CcConstraint': CcConstraint option
    ShapeConstraint': PartnershipShapeConstraint option
    // No Cards' (cf. HandScenario.HandScenario).
    CustomPredicate': (Hand * Hand -> bool) option }
    with
    static member Make partnership = {
        Partnership' = partnership
        HcpConstraint' = None
        CcConstraint' = None
        ShapeConstraint' = None
        CustomPredicate' = None }
    member this.Partnership = this.Partnership'
    member this.HcpConstraint = this.HcpConstraint'
    member this.CcConstraint = this.CcConstraint'
    member this.ShapeConstraint = this.ShapeConstraint'
    member this.CustomPredicate = this.CustomPredicate'
    member this.Matches (hand1:Hand, hand2:Hand) =
        let hcpMatches = match this.HcpConstraint with | Some hcpConstraint -> hcpConstraint.Matches (hand1.Hcp + hand2.Hcp) | None -> true
        let ccMatches = match this.CcConstraint with | Some ccConstraint -> ccConstraint.Matches (hand1.Cc + hand2.Cc) | None -> true
        let shapeMatches = match this.ShapeConstraint with | Some shapeConstraint -> shapeConstraint.Matches (hand1, hand2) | None -> true
        let customMatches = match this.CustomPredicate with | Some customPredicate -> customPredicate (hand1, hand2) | None -> true
        hcpMatches && ccMatches && shapeMatches && customMatches
    member this.Text =
        let hcpText = this.HcpConstraint |> Option.map (fun hcpConstraint -> hcpConstraint.Text)
        let ccText = this.CcConstraint |> Option.map (fun ccConstraint -> ccConstraint.Text)
        let shapeText = this.ShapeConstraint |> Option.map (fun shapeConstraint -> shapeConstraint.Text)
        let customPredicateText = this.CustomPredicate |> Option.map (fun _ -> "with custom predicate")
        let constraintsText =
            match [ hcpText; ccText; shapeText; customPredicateText ] |> List.choose id with
            | h :: t -> h :: t |> String.concat " | "
            | [] -> "(none)" // should never happen
        $"{this.Partnership.ShortText}: {constraintsText}"

[<RequireQualifiedAccess>]
type PartnershipProp =
    | Partnership of Partnership
    | HcpConstraint of PartnershipHcpConstraint
    | CcConstraint of CcConstraint
    | ShapeConstraint of PartnershipShapeConstraint
    | CustomPredicate of (Hand * Hand -> bool)

exception OtherPartnershipPropIsPartnershipException of Partnership
exception PartnershipPropAlreadySpecifiedException of Partnership * PartnershipProp

let private folder (state:PartnershipScenario) prop =
    match prop with
    | PartnershipProp.Partnership partnership -> raise (OtherPartnershipPropIsPartnershipException partnership) // should never happen as would not compile
    | PartnershipProp.HcpConstraint _ when state.HcpConstraint |> Option.isSome -> raise (PartnershipPropAlreadySpecifiedException (state.Partnership, prop))
    | PartnershipProp.CcConstraint _ when state.CcConstraint |> Option.isSome -> raise (PartnershipPropAlreadySpecifiedException (state.Partnership, prop))
    | PartnershipProp.ShapeConstraint _ when state.ShapeConstraint |> Option.isSome -> raise (PartnershipPropAlreadySpecifiedException (state.Partnership, prop))
    | PartnershipProp.CustomPredicate _ when state.CustomPredicate |> Option.isSome -> raise (PartnershipPropAlreadySpecifiedException (state.Partnership, prop))
    | PartnershipProp.HcpConstraint hcpConstraint -> { state with HcpConstraint' = Some hcpConstraint }
    | PartnershipProp.CcConstraint ccConstraint -> { state with CcConstraint' = Some ccConstraint }
    | PartnershipProp.ShapeConstraint shapeConstraint -> { state with ShapeConstraint' = Some shapeConstraint }
    | PartnershipProp.CustomPredicate customPredicate -> { state with CustomPredicate' = Some customPredicate }

exception NoPartnershipPropsException
exception NoOtherPartnershipPropsException of Partnership
exception FirstPartnershipPropNotPartnershipException of PartnershipProp

type PartnershipScenarioBuilder() =
    member inline _.Yield (()) = ()

    member _.Run (props: PartnershipProp list) =
        match props |> List.rev with
        | [] -> raise NoPartnershipPropsException // should never happen as would not compile
        | PartnershipProp.Partnership partnership :: otherProps ->
            match otherProps with
            | [] -> raise (NoOtherPartnershipPropsException partnership)
            | _ ->
                otherProps
                |> List.fold folder (PartnershipScenario.Make partnership)
        | firstProp :: _ -> raise (FirstPartnershipPropNotPartnershipException firstProp) // should never happen as would not compile

    [<CustomOperation("partnership")>]
    member inline _.Position ((), partnership) = [ PartnershipProp.Partnership partnership ]

    [<CustomOperation("hcp")>]
    member inline _.HcpConstraint (props, hcpConstraint) = PartnershipProp.HcpConstraint hcpConstraint :: props

    [<CustomOperation("cc")>]
    member inline _.CcConstraint (props, ccConstraint) = PartnershipProp.CcConstraint ccConstraint :: props

    [<CustomOperation("shape")>]
    member inline _.ShapeConstraint (props, shapeConstraint) = PartnershipProp.ShapeConstraint shapeConstraint :: props

    [<CustomOperation("customPredicate")>]
    member inline _.CustomPredicate (props, customPredicate) = PartnershipProp.CustomPredicate customPredicate :: props

let partnershipScenario = PartnershipScenarioBuilder ()
