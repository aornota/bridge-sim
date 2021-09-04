module Aornota.BridgeSim.DevConsole.Scratch.Scenario.FiveFourMajor

open Aornota.BridgeSim.Common.Console
open Aornota.BridgeSim.Common.Mathy
open Aornota.BridgeSim.Dds.Interop.Dds
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

type private FiveFourMajorStrategy = | SimpleStayman54 | ExtendedStayman54 | Transfer54 with
    member this.Text =
        match this with
        | SimpleStayman54 -> "Simple Stayman (5-4)" // Stayman | 4M over 2M | 3NT over 2D
        | ExtendedStayman54 -> "Extended Stayman (5-4)" // Stayman | 4M over 2M | 3M (5-card) over 2D, then 3NT (if doubleton) or 4M
        | Transfer54 -> "Transfer-only (5-4)" // transfer (to 5-card major) | 3NT over forced 2M | correct to 4M unless doubleton

let private generateFiveFourMajorSimulation i : (int * Simulation<FiveFourMajorStrategy>) option = // 1NT opening where responder has 13-15 HCP and exactly 5-4 in the majors
    let simulation' deal (strategyContracts:(FiveFourMajorStrategy * Contract) list) =
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
                let openerSpades, openerHearts, _, _ = hand.SuitCounts
                let partnerHand = deal.Hand(position.Partner)
                match partnerHand.Hcp with
                | hcp when hcp >= 13<hcp> && hcp <= 15<hcp> -> // game (but probably not slam) combined strength of 25-29
                    let partnerSpades, partnerHearts, _, _ = partnerHand.SuitCounts
                    match partnerSpades, partnerHearts with
                    // Responder is 5=4...
                    | 5, 4 when openerHearts >= 4 -> // always 4H (by opener) for SimpleStayman54 and ExtendedStayman54 | either 4S (by opener) or 3NT (by opener) for Transfer54
                        let strategyContracts =
                            [
                                SimpleStayman54, undoubledContract FourLevel (Suit Heart) position // 1NT | 2C | 2H | 4H | - (opener is declarer)
                                ExtendedStayman54, undoubledContract FourLevel (Suit Heart) position // 1NT | 2C | 2H | 4H | - (opener is declarer)
                                if openerSpades >= 3 then
                                    Transfer54, undoubledContract FourLevel (Suit Spade) position // 1NT | 2H | 2S | 3NT | 4S | - (opener is declarer)
                                else Transfer54, undoubledContract ThreeLevel NoTrump position // // 1NT | 2H | 2S | 3NT | - (opener is declarer)
                            ]
                        simulation' deal strategyContracts
                    (* Exclude deal where contract would be the same for all strategies:
                    | 5, 4 when openerSpades >= 4 -> // always 4S (by opener) for all strategies
                        let strategyContracts =
                            [
                                SimpleStayman54, undoubledContract FourLevel (Suit Spade) position // 1NT | 2C | 2S | 4S | - (opener is declarer)
                                ExtendedStayman54, undoubledContract FourLevel (Suit Spade) position // 1NT | 2C | 2S | 4S | - (opener is declarer)
                                Transfer54, undoubledContract FourLevel (Suit Spade) position // 1NT | 2H | 2S | 3NT | 4S | - (opener is declarer)
                            ]
                        simulation' deal strategyContracts *)
                    | 5, 4 when openerSpades < 4 -> // always 3NT (by opener) for SimpleStayman54 | either 4S (by partner) or 3NT (by opener) for ExtendedStayman54 | either 4S (by opener) or 3NT (by opener) for Transfer54
                        let strategyContracts =
                            [
                                SimpleStayman54, undoubledContract ThreeLevel NoTrump position // 1NT | 2C | 2D | 3NT | - (opener is declarer)
                                if openerSpades >= 3 then
                                    ExtendedStayman54, undoubledContract FourLevel (Suit Spade) position.Partner // 1NT | 2C | 2D | 3S | 4S | - (responder is declarer)
                                else ExtendedStayman54, undoubledContract ThreeLevel NoTrump position // // 1NT | 2C | 2D | 3S | 3NT | - (opener is declarer)
                                if openerSpades >= 3 then
                                    Transfer54, undoubledContract FourLevel (Suit Spade) position // 1NT | 2H | 2S | 3NT | 4S | - (opener is declarer)
                                else Transfer54, undoubledContract ThreeLevel NoTrump position // // 1NT | 2H | 2S | 3NT | - (opener is declarer)
                            ]
                        simulation' deal strategyContracts
                    // Responder is 4=5...
                    (* Exclude deal where contract would be the same for all strategies:
                    | 4, 5 when openerHearts >= 4 -> // always 4H (by opener) for all strategies
                        let strategyContracts =
                            [
                                SimpleStayman54, undoubledContract FourLevel (Suit Heart) position // 1NT | 2C | 2H | 4H | - (opener is declarer)
                                ExtendedStayman54, undoubledContract FourLevel (Suit Heart) position // 1NT | 2C | 2H | 4H | - (opener is declarer)
                                Transfer54, undoubledContract FourLevel (Suit Heart) position // 1NT | 2D | 2H | 3NT | 4H | - (opener is declarer)
                            ]
                        simulation' deal strategyContracts *)
                    | 4, 5 when openerHearts < 4 && openerSpades >= 4 -> // always 4S (by opener) for SimpleStayman54 and ExtendedStayman54 | either 4H (by opener) or 3NT (by opener) for Transfer54
                        let strategyContracts =
                            [
                                SimpleStayman54, undoubledContract FourLevel (Suit Spade) position // 1NT | 2C | 2S | 4S | - (opener is declarer)
                                ExtendedStayman54, undoubledContract FourLevel (Suit Spade) position // 1NT | 2C | 2S | 4S | - (opener is declarer)
                                if openerHearts >= 3 then
                                    Transfer54, undoubledContract FourLevel (Suit Heart) position // 1NT | 2D | 2H | 3NT | 4H | - (opener is declarer)
                                else Transfer54, undoubledContract ThreeLevel NoTrump position // // 1NT | 2D | 2HN | 3NT | - (opener is declarer)
                            ]
                        simulation' deal strategyContracts
                    | 4, 5 -> // always 3NT (by opener) for SimpleStayman54 | either 4H (by partner) or 3NT (by opener) for ExtendedStayman54 | either 4H (by opener) or 3NT (by opener) for Transfer54
                        let strategyContracts =
                            [
                                SimpleStayman54, undoubledContract ThreeLevel NoTrump position // 1NT | 2C | 2D | 3NT | - (opener is declarer)
                                if openerHearts >= 3 then
                                    ExtendedStayman54, undoubledContract FourLevel (Suit Heart) position.Partner // 1NT | 2C | 2D | 3H | 4H | - (responder is declarer)
                                else ExtendedStayman54, undoubledContract ThreeLevel NoTrump position // // 1NT | 2C | 2D | 3H | 3NT | - (opener is declarer)
                                if openerHearts >= 3 then
                                    Transfer54, undoubledContract FourLevel (Suit Heart) position // 1NT | 2D | 2H | 3NT | 4H | - (opener is declarer)
                                else Transfer54, undoubledContract ThreeLevel NoTrump position // // 1NT | 2D | 2H | 3NT | - (opener is declarer)
                            ]
                        simulation' deal strategyContracts
                    | _ -> None
                | _ -> None
            | _ -> check deal remaining
    check (Deal.MakeRandom()) [ North ; East; South ; West ]

let run (mode:Mode) withDoubleDummy count =
    if count = 0 then raise CountMustBeGreaterThanZeroException
    let simulationDescription = "Weak 1NT opening where responder has 13-15 HCP and 5-4 in the majors"
    let subDir = "weak-1NT-when-responder-has-13-to-15-hcp-and-5-4-majors"
    let scenarioDir = scenarioDir subDir
    let conditionalText = if withDoubleDummy then "generating and analyzing" else "generating"
    let countText = if count = 1 then "deal" else "deals"
    writeNewLine $"{simulationDescription} -> {conditionalText} {count} matching {countText}:\n" ConsoleColor.Magenta
    if not mode.Display then writeBlankLine ()
    let generator = Seq.initInfinite generateFiveFourMajorSimulation |> Seq.choose id
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
        let simpleStaymanMeanNotVulnerable, simpleStaymanMeanVulnerable = Mean<score>.FromList(scores SimpleStayman54 NotVulnerable), Mean<score>.FromList(scores SimpleStayman54 Vulnerable)
        let extendedStaymanMeanNotVulnerable, extendedStaymanMeanVulnerable = Mean<score>.FromList(scores ExtendedStayman54 NotVulnerable), Mean<score>.FromList(scores ExtendedStayman54 Vulnerable)
        let transferOnlyMeanNotVulnerable, transferOnlyMeanVulnerable = Mean<score>.FromList(scores Transfer54 NotVulnerable), Mean<score>.FromList(scores Transfer54 Vulnerable)
        if not mode.Display then writeBlankLine ()
        writeNewLine "\tMean scores per strategy:\n" ConsoleColor.DarkGray
        writeNewLine $"\t{SimpleStayman54.Text} -> {simpleStaymanMeanNotVulnerable.Mean} (non-vul.) | {simpleStaymanMeanVulnerable.Mean} (vul.)" ConsoleColor.Gray
        writeNewLine $"\t{ExtendedStayman54.Text} -> {extendedStaymanMeanNotVulnerable.Mean} (non-vul.) | {extendedStaymanMeanVulnerable.Mean} (vul.)" ConsoleColor.Gray
        writeNewLine $"\t{Transfer54.Text} -> {transferOnlyMeanNotVulnerable.Mean} (non-vul.) | {transferOnlyMeanVulnerable.Mean} (vul.)" ConsoleColor.Gray
        writeBlankLine ()
    // TODO-NMB: Additional analysis, e.g. how often 6M/6NT possible for each combined HCP (25-29)?...
    let conditionalText = if withDoubleDummy then "generated and analyzed" else "generated"
    if mode.Display then writeNewLine $"{simulationDescription} -> {conditionalText} {count} matching {countText} (from {iMax} random deal/s)" ConsoleColor.DarkYellow
    else writeNewLine $"{simulationDescription} -> {conditionalText} {count} matching {countText} (from {iMax} random deal/s) in {(DateTime.UtcNow - start).TotalSeconds} seconds" ConsoleColor.DarkYellow
