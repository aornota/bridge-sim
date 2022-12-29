module Aornota.BridgeSim.Domain.Simulation.Scenario

open Aornota.BridgeSim.Domain.Auction
open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Formatting.Auction
open Aornota.BridgeSim.Domain.Formatting.Core
open Aornota.BridgeSim.Domain.Simulation.HandScenario
open Aornota.BridgeSim.Domain.Simulation.PartnershipScenario

// See also https://sleepyfran.github.io/blog/posts/fsharp/ce-in-fsharp/.async

type ContractType = | Specific of Level * Strain | AnyGame | NtGame | MajorGame | MinorGame

type Declarer = | Position of Position | Partnership of Partnership

type VulnerabilityScoring = | NotVulnerableOnly | VulnerableOnly | NotVulnerableAndVulnerable

type DoubledScoring = | UndoubledOnly | DoubledOnly | UndoubledAndDoubled // TODO-NMB: Extend for Redoubled?...

type ScenarioScoring = {
    VulnerabilityScoring: VulnerabilityScoring
    DoubledScoring: DoubledScoring }

type ScenarioContract = {
    ContractType: ContractType
    Declarer: Declarer
    ScenarioScoring: ScenarioScoring option }
    with
    member this.Contracts =
        let threeNt declarers = declarers |> List.map (fun declarer -> Contract (ThreeLevel, NoTrump, Undoubled, declarer))
        let fourSpades declarers = declarers |> List.map (fun declarer -> Contract (FourLevel, Suit Spade, Undoubled, declarer))
        let fourHearts declarers = declarers |> List.map (fun declarer -> Contract (FourLevel, Suit Heart, Undoubled, declarer))
        let fiveDiamonds declarers = declarers |> List.map (fun declarer -> Contract (FiveLevel, Suit Diamond, Undoubled, declarer))
        let fiveClubs declarers = declarers |> List.map (fun declarer -> Contract (FiveLevel, Suit Club, Undoubled, declarer))
        let majorGame declarers = fourSpades declarers @ fourHearts declarers
        let minorGame declarers = fiveDiamonds declarers @ fiveClubs declarers
        let anyGame declarers = threeNt declarers @ majorGame declarers @ minorGame declarers
        let declarers = match this.Declarer with | Position position -> [ position ] | Partnership NorthSouth -> [ North; South ] | Partnership EastWest -> [ East; West ]
        match this.ContractType with
        | Specific (level, strain) -> declarers |> List.map (fun declarer -> Contract (level, strain, Undoubled, declarer))
        | AnyGame -> anyGame declarers
        | NtGame -> threeNt declarers
        | MajorGame -> majorGame declarers
        | MinorGame -> minorGame declarers
    member this.ScoringContracts =
        match this.ScenarioScoring with
        | Some scenarioScoring ->
            let doubled withUndoubled =
                this.Contracts
                |> List.map (fun contract ->
                    match contract with
                    | Contract (level, strain, _ , declarer) ->
                        let doubled = Contract (level, strain, Doubled, declarer)
                        if withUndoubled then [ contract; doubled ] else [ doubled ]
                    | PassedOut -> [])
                |> List.collect id
            match scenarioScoring.DoubledScoring with | UndoubledOnly -> this.Contracts | DoubledOnly -> doubled false | UndoubledAndDoubled -> doubled true
        | None -> []
    member this.Text =
        let contracts = match this.ScoringContracts with | h :: t -> h :: t | [] -> this.Contracts
        let contractsText =
            contracts
            |> List.choose (fun contract ->
                match contract with
                | Contract (level, strain, stakes, _) -> Some $"{level.ShortText}{strain.ShortText}{stakes.ShortText}"
                | PassedOut -> None)
            |> String.concat " and "
        let declarerText =
            match this.Declarer with
            | Position position -> position.Text
            | Partnership NorthSouth -> $"{North.Text} or {South.Text}"
            | Partnership EastWest -> $"{East.Text} or {West.Text}"
        let vulnerabilitiesText =
            match this.ScenarioScoring with
            | Some scenarioScoring ->
                let vulnerabilitiesText =
                    match scenarioScoring.VulnerabilityScoring with
                    | NotVulnerableOnly -> NotVulnerable.ShortTextLower
                    | VulnerableOnly -> Vulnerable.ShortTextLower
                    | NotVulnerableAndVulnerable -> $"{NotVulnerable.ShortTextLower} and {Vulnerable.ShortTextLower}"
                $" ({vulnerabilitiesText})"
            | None -> ""
        $"{contractsText} by {declarerText}{vulnerabilitiesText}"

exception EmptyListForContractsException
exception DuplicateContractsException of ScenarioContract list * int

type Scenario = private { // TODO-NMB: Add Description'?...
    Contracts': ScenarioContract list
    Dealer': Position option
    Vulnerabilities': (Vulnerability * Vulnerability) option
    NorthSouthScenario': PartnershipScenario option
    NorthScenario': HandScenario option
    SouthScenario': HandScenario option
    EastWestScenario': PartnershipScenario option
    EastScenario': HandScenario option
    WestScenario': HandScenario option
    CustomPredicate': (Hand * Hand * Hand * Hand -> bool) option }
    with
    static member Make scenarioContracts =
        match scenarioContracts with
        | [] -> raise EmptyListForContractsException
        | _ ->
            let contracts = scenarioContracts |> List.map (fun (scenarioContract:ScenarioContract) -> scenarioContract.Contracts) |> List.collect id
            match contracts.Length - (contracts |> Set.ofList).Count with
            | 0 -> {
                Contracts' = scenarioContracts
                Dealer' = None
                Vulnerabilities' = None
                NorthSouthScenario' = None
                NorthScenario' = None
                SouthScenario' = None
                EastWestScenario' = None
                EastScenario' = None
                WestScenario' = None
                CustomPredicate' = None }
            | duplicateCount -> raise (DuplicateContractsException (scenarioContracts, duplicateCount))
    member this.Contracts = this.Contracts'
    member this.Dealer = this.Dealer'
    member this.Vulnerabilities = this.Vulnerabilities'
    member this.NorthSouthScenario = this.NorthSouthScenario'
    member this.NorthScenario = this.NorthScenario'
    member this.SouthScenario = this.SouthScenario'
    member this.EastWestScenario = this.EastWestScenario'
    member this.EastScenario = this.EastScenario'
    member this.WestScenario = this.WestScenario'
    member this.CustomPredicate = this.CustomPredicate'
    member this.Text =
        let contractsText = this.Contracts |> List.map (fun contract -> $"\n{contract.Text}") |> String.concat ""
        let dealerText = this.Dealer |> Option.map (fun position -> $"Dealer: {position.Text}")
        let vulnerabilitiesText =
            this.Vulnerabilities |> Option.map (fun (northSouth, eastWest) ->
                match northSouth, eastWest with
                | NotVulnerable, NotVulnerable -> "neither vulnerable"
                | Vulnerable, NotVulnerable -> $"{NorthSouth.ShortText} vulnerable"
                | NotVulnerable, Vulnerable -> $"{EastWest.ShortText} vulnerable"
                | Vulnerable, Vulnerable -> "both vulnerable")
        let headerText =
            match [ dealerText; vulnerabilitiesText ] |> List.choose id with
            | h :: t -> h :: t |> String.concat " | "
            | [] -> ""
        let northSouthScenarioText = this.NorthSouthScenario |> Option.map (fun scenario -> $"\n{scenario.Text}")
        let northScenarioText = this.NorthScenario |> Option.map (fun scenario -> $"\n{scenario.Text}")
        let southScenarioText = this.SouthScenario |> Option.map (fun scenario -> $"\n{scenario.Text}")
        let eastWestScenarioText = this.EastWestScenario |> Option.map (fun scenario -> $"\n{scenario.Text}")
        let eastScenarioText = this.EastScenario |> Option.map (fun scenario -> $"\n{scenario.Text}")
        let westScenarioText = this.WestScenario |> Option.map (fun scenario -> $"\n{scenario.Text}")
        let scenariosText =
            match [ northSouthScenarioText; northScenarioText; southScenarioText; eastWestScenarioText; eastScenarioText; westScenarioText ] |> List.choose id with
            | h :: t -> h :: t |> String.concat ""
            | [] -> "(none)" // should never happen
        let customPredicateText = this.CustomPredicate |> Option.map (fun _ -> "\nwith custom predicate")
        $"{headerText}{contractsText}{scenariosText}{customPredicateText}"

[<RequireQualifiedAccess>]
type Prop =
    | Contracts of ScenarioContract list
    | Dealer of Position
    | Vulnerabilities of Vulnerability * Vulnerability
    | PartnershipScenario of PartnershipScenario
    | HandScenario of HandScenario
    | CustomPredicate of (Hand * Hand * Hand * Hand -> bool)

exception OtherPropIsContractsException of ScenarioContract list
exception PropAlreadySpecifiedException of ScenarioContract list * Prop

let private folder (state:Scenario) prop =
    match prop with
    | Prop.Contracts scenarioContracts -> raise (OtherPropIsContractsException scenarioContracts) // should never happen as would not compile
    | Prop.Dealer _ when state.Dealer |> Option.isSome -> raise (PropAlreadySpecifiedException (state.Contracts, prop))
    | Prop.Vulnerabilities _ when state.Vulnerabilities |> Option.isSome -> raise (PropAlreadySpecifiedException (state.Contracts, prop))
    | Prop.PartnershipScenario scenario when
        (scenario.Partnership = NorthSouth && state.NorthSouthScenario |> Option.isSome)
        || (scenario.Partnership = EastWest && state.EastWestScenario |> Option.isSome) -> raise (PropAlreadySpecifiedException (state.Contracts, prop))
    | Prop.HandScenario scenario when
        (scenario.Position = North && state.NorthScenario |> Option.isSome)
        || (scenario.Position = South && state.SouthScenario |> Option.isSome)
        || (scenario.Position = East && state.EastScenario |> Option.isSome)
        || (scenario.Position = West && state.WestScenario |> Option.isSome) -> raise (PropAlreadySpecifiedException (state.Contracts, prop))
    | Prop.CustomPredicate _ when state.CustomPredicate |> Option.isSome -> raise (PropAlreadySpecifiedException (state.Contracts, prop))
    | Prop.Dealer dealer -> { state with Dealer' = Some dealer }
    | Prop.Vulnerabilities (northSouth, eastWest) -> { state with Vulnerabilities' = Some (northSouth, eastWest) }
    | Prop.PartnershipScenario scenario ->
        match scenario.Partnership with
        | NorthSouth -> { state with NorthSouthScenario' = Some scenario }
        | EastWest -> { state with EastWestScenario' = Some scenario }
    | Prop.HandScenario scenario ->
        match scenario.Position with
        | North -> { state with NorthScenario' = Some scenario }
        | South -> { state with SouthScenario' = Some scenario }
        | East -> { state with EastScenario' = Some scenario }
        | West -> { state with WestScenario' = Some scenario }
    | Prop.CustomPredicate customPredicate -> { state with CustomPredicate' = Some customPredicate }

exception NoPartnershipOrHandScenariosException of ScenarioContract list
// TODO-NMB: More validation exceptions...

let validate (state:Scenario) =
    match state.NorthSouthScenario, state.NorthScenario, state.SouthScenario, state.EastWestScenario, state.EastScenario, state.WestScenario with
    | None, None, None, None, None, None -> raise (NoPartnershipOrHandScenariosException state.Contracts)
    | _ -> ()
    (* TODO-NMB: More validation...
        -- Check unique Cards for HandScenarios...
        -- Other cross-Scenario checks... *)
    state

exception NoPropsException
exception NoOtherPropsException of ScenarioContract list
exception FirstPropNotContractsException of Prop

type ScenarioBuilder() =
    member inline _.Yield (()) = ()

    member _.Run (props: Prop list) =
        match props |> List.rev with
        | [] -> raise NoPropsException // should never happen as would not compile
        | Prop.Contracts scenarioContracts :: otherProps ->
            match otherProps with
            | [] -> raise (NoOtherPropsException scenarioContracts)
            | _ ->
                otherProps
                |> List.fold folder (Scenario.Make scenarioContracts)
                |> validate
        | firstProp :: _ -> raise (FirstPropNotContractsException firstProp) // should never happen as would not compile

    [<CustomOperation("contracts")>]
    member inline _.Contracts ((), scenarioContracts) = [ Prop.Contracts scenarioContracts ]

    [<CustomOperation("dealer")>]
    member inline _.Dealer (props, dealer) = Prop.Dealer dealer :: props

    [<CustomOperation("vulnerabilities")>]
    member inline _.Vulnerabilities (props, (northSouth, eastWest)) = Prop.Vulnerabilities (northSouth, eastWest) :: props

    [<CustomOperation("partnership")>]
    member inline _.PartnershipScenario (props, partnershipScenario) = Prop.PartnershipScenario partnershipScenario :: props

    [<CustomOperation("hand")>]
    member inline _.HandScenario (props, handScenario) = Prop.HandScenario handScenario :: props

    [<CustomOperation("customPredicate")>]
    member inline _.CustomPredicate (props, customPredicate) = Prop.CustomPredicate customPredicate :: props

let scenario = ScenarioBuilder ()
