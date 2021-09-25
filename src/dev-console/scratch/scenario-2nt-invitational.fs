module Aornota.BridgeSim.DevConsole.Scratch.Scenario.TwoNtInvitational

open Aornota.BridgeSim.Common.Console
open Aornota.BridgeSim.Common.Mathy
open Aornota.BridgeSim.Dds.Interop.Dds
open Aornota.BridgeSim.DevConsole.Common
open Aornota.BridgeSim.DevConsole.Scratch.Dds
open Aornota.BridgeSim.DevConsole.Scratch.Simulation
open Aornota.BridgeSim.Domain.Auction
open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Evaluation.Core
open Aornota.BridgeSim.Domain.Formatting.Auction
open Aornota.BridgeSim.Domain.Formatting.Deal
open Aornota.BridgeSim.Domain.Random.Deal
open Aornota.BridgeSim.Domain.Scoring.Auction

open System
open System.IO

type private TwoNtInvitationalStrategy = | OldSchool | NewSchool | TooCoolForSchool with
    member this.Text =
        match this with
        | OldSchool -> "Old school (2NT invitational)" // 2NT (invitational) with 11-12; opener accepts with 14
        | NewSchool -> "New school (2NT invitational)" // 2NT (invitational) with 11-12; opener accepts with 14, or with 13 and a 5-card suit
        | TooCoolForSchool -> "Too cool for school (no invitations)" // pass with 11; 3NT with 12 and a 5-card (minor) suit

let private generateTwoNtInvitationalSimulation i : (int * Simulation<TwoNtInvitationalStrategy>) option = // 1NT opening where responder has 11-12 HCP balanced and no 4-card or longer major
    let simulation' deal (strategyContracts:(TwoNtInvitationalStrategy * Contract) list) =
        match strategyContracts |> List.groupBy fst |> List.filter (fun (_, list) -> list.Length > 1) with | [] -> () | _ -> raise MultipleContractsForOneOrMoreStrategyException
        if strategyContracts.Length <> 3 then raise NoContractForOneOrMoreStrategyException
        simulation i deal strategyContracts
    let undoubledContract level strain position = Contract (level, strain, Undoubled, position)
    let rec check (deal:Deal) positions =
        match positions with
        | [] -> None
        | position :: remaining ->
            let hand = deal.Hand(position)
            match hand.ShapeCategory, hand.Hcp with
            | Balanced, hcp when hcp >= 12<hcp> && hcp <= 14<hcp> -> // 1NT opener
                let partnerHand = deal.Hand(position.Partner)
                let partnerSpades, partnerHearts, _, _ = partnerHand.SuitCounts
                match partnerHand.ShapeCategory, partnerHand.Hcp with
                | Balanced, partnerHcp when (partnerHcp = 11<hcp> || partnerHcp = 12<hcp>) && partnerSpades < 4 && partnerHearts < 4 -> // balanced and invitational (combined strength of 23-26) with no 4-card or longer major
                    // Exclude deal where contract would be the same - 3MT (by opener) - for all strategies:
                    if hcp = 14<hcp> && partnerHcp = 12<hcp> && partnerHand.Shape = FiveThreeThreeTwo then None
                    else
                        let strategyContracts = // either 2NT (by opener) or 3NT (by opener) for OldSchool and NewSchool | either 1NT (by opener) or 3NT (by opener) for TooCoolForSchool
                            [
                                if hcp = 14<hcp> then
                                    OldSchool, undoubledContract ThreeLevel NoTrump position // 1NT | 2NT | 3NT | - (opener is declarer)
                                else OldSchool, undoubledContract TwoLevel NoTrump position // 1NT | 2NT | - (opener is declarer)
                                if hcp = 14<hcp> || (hcp = 13<hcp> && hand.Shape = FiveThreeThreeTwo) then
                                    NewSchool, undoubledContract ThreeLevel NoTrump position // 1NT | 2NT | 3NT | - (opener is declarer)
                                else NewSchool, undoubledContract TwoLevel NoTrump position // 1NT | 2NT | - (opener is declarer)
                                if partnerHcp = 12<hcp> && partnerHand.Shape = FiveThreeThreeTwo then
                                    TooCoolForSchool, undoubledContract ThreeLevel NoTrump position // 1NT | 3NT | - (opener is declarer)
                                else TooCoolForSchool, undoubledContract OneLevel NoTrump position // 1NT | - (opener is declareer)
                            ]
                        simulation' deal strategyContracts
                | _ -> None
            | _ -> check deal remaining
    check (Deal.MakeRandom()) [ North ; East; South ; West ]

let run (mode:Mode) withDoubleDummy count =
    if count = 0 then raise CountMustBeGreaterThanZeroException
    let simulationDescription = "Weak 1NT opening where responder has 11-12 HCP balanced and no 4-card or longer major"
    let subDir = "weak-1NT-when-responder-has-11-to-12-hcp-balanced-and-no-4-card-or-longer-major"
    let scenarioDir = scenarioDir subDir
    let conditionalText = if withDoubleDummy then "generating and analyzing" else "generating"
    let countText = if count = 1 then "deal" else "deals"
    writeNewLine $"{simulationDescription} -> {conditionalText} {count} matching {countText}:\n" ConsoleColor.Magenta
    if not mode.Display then writeBlankLine ()
    let generator = Seq.initInfinite generateTwoNtInvitationalSimulation|> Seq.choose id
    let mutable iMax = 0
    let mutable simulations = []
    let start = DateTime.UtcNow
    generator |> Seq.take count |> Seq.iter (fun (i, simulation) ->
        iMax <- i
        if mode.Display then
            writeBlankLine ()
            simulation.Deal.Summary(false, true) |> List.iter (fun line -> write $"\t{line}\n" ConsoleColor.Cyan)
        let simulation =
            if withDoubleDummy then
                let results = calculateDoubleDummy simulation.Deal
                if mode.Display then
                    writeDoubleDummyResults results
                    writeBlankLine ()
                simulation.WithResults(results)
            else simulation
        simulations <- simulation :: simulations
        if mode.Display then
            writeNewLine "\tStrategy contracts:\n" ConsoleColor.DarkGray
            simulation.StrategyContracts |> List.iter (fun strategyContract ->
                let scoresText = match strategyContract.Scores with | Some (notVulnerable, vulnerable) -> $" = {notVulnerable} (non-vul.) | {vulnerable} (vul.)" | None -> ""
                writeNewLine $"\t{strategyContract.Strategy.Text} -> {strategyContract.Contract.ShortText}{scoresText}" ConsoleColor.Gray)
            writeBlankLine ()
        if mode.Save then
            let fileName = $"{Guid.NewGuid()}.json"
            File.WriteAllText(Path.Combine(scenarioDir, fileName), simulation.ToJson())
            if mode.Display then writeNewLine $"\tSaved to .../{subDir}/{fileName}\n" ConsoleColor.DarkCyan
        if mode.Interactive then
            writeNewLine "Press any key to continue..." ConsoleColor.DarkMagenta
            Console.ReadKey () |> ignore
            writeBlankLine ()
        else if not mode.Display then write "." ConsoleColor.DarkMagenta)
    if withDoubleDummy then
        let scores strategy vulnerability =
            simulations
            |> List.map (fun simulation ->
                match simulation.StrategyContracts |> List.tryFind (fun strategyContract -> strategyContract.Strategy = strategy) with
                | Some strategyContract ->
                    match strategyContract.Scores, vulnerability with
                    | Some (notVulnerable, _), NotVulnerable -> notVulnerable
                    | Some (_, vulnerable), Vulnerable -> vulnerable
                    | _ -> raise NoScoreForStrategyException
                | None -> raise NoContractForStrategyException)
        let oldSchoolMeanNotVulnerable, oldSchoolMeanVulnerable = Mean<score>.FromList(scores OldSchool NotVulnerable), Mean<score>.FromList(scores OldSchool Vulnerable)
        let newSchoolMeanNotVulnerable, newSchoolMeanVulnerable = Mean<score>.FromList(scores NewSchool NotVulnerable), Mean<score>.FromList(scores NewSchool Vulnerable)
        let tooCoolForSchoolMeanNotVulnerable, tooCoolForSchoolMeanVulnerable = Mean<score>.FromList(scores TooCoolForSchool NotVulnerable), Mean<score>.FromList(scores TooCoolForSchool Vulnerable)
        if not mode.Display then writeBlankLine ()
        writeNewLine "\tMean scores per strategy:\n" ConsoleColor.DarkGray
        writeNewLine $"\t{OldSchool.Text} -> {oldSchoolMeanNotVulnerable.Mean} (non-vul.) | {oldSchoolMeanVulnerable.Mean} (vul.)" ConsoleColor.Gray
        writeNewLine $"\t{NewSchool.Text} -> {newSchoolMeanNotVulnerable.Mean} (non-vul.) | {newSchoolMeanVulnerable.Mean} (vul.)" ConsoleColor.Gray
        writeNewLine $"\t{TooCoolForSchool.Text} -> {tooCoolForSchoolMeanNotVulnerable.Mean} (non-vul.) | {tooCoolForSchoolMeanVulnerable.Mean} (vul.)" ConsoleColor.Gray
        writeBlankLine ()
    // TODO-NMB: Additional analysis, e.g. how often 2NT/3NT possible for each combined HCP (23-26)?...
    let conditionalText = if withDoubleDummy then "generated and analyzed" else "generated"
    if mode.Display then writeNewLine $"{simulationDescription} -> {conditionalText} {count} matching {countText} (from {iMax} random deal/s)" ConsoleColor.DarkYellow
    else writeNewLine $"{simulationDescription} -> {conditionalText} {count} matching {countText} (from {iMax} random deal/s) in {(DateTime.UtcNow - start).TotalSeconds} seconds" ConsoleColor.DarkYellow
