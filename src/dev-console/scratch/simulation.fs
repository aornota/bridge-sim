module Aornota.BridgeSim.DevConsole.Scratch.Simulation

open Aornota.BridgeSim.Common.Console
open Aornota.BridgeSim.Common.Mathy
open Aornota.BridgeSim.Dds.Interop.Dds
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
open Thoth.Json.Net

exception CannotDeserializeSimulationException of json:string * error:string

type private StrategyContract<'a> = { Strategy : 'a ; Contract : Contract ; Scores : (int<score> * int<score>) option } with
    static member Make(strategy, contract) = { Strategy = strategy ; Contract = contract ; Scores = None }
    member this.WithScores(scores) = { this with Scores = Some scores }

type private Simulation<'a> = { Deal : Deal ; StrategyContracts : StrategyContract<'a> list ; Results : DoubleDummyResults option } with
    static member Make(deal, strategyContracts) = { Deal = deal ; StrategyContracts = strategyContracts ; Results = None }
    static member FromJson<'a> json = match Decode.Auto.fromString<Simulation<'a>> json with | Ok value -> value | Error error -> raise (CannotDeserializeSimulationException (json, error))
    member this.ToJson<'a>() = Encode.Auto.toString<Simulation<'a>> (4, this)
    member this.WithResults(results:DoubleDummyResults) =
        let strategyContracts =
            this.StrategyContracts
            |> List.map (fun strategyContract ->
                let scores =
                    match strategyContract.Contract with
                    | Contract (_, strain, _, position) ->
                        let tricks = results.Tricks(position, strain)
                        strategyContract.Contract.DuplicateScore(Some (NotVulnerable, tricks)), strategyContract.Contract.DuplicateScore(Some (Vulnerable, tricks))
                    | PassedOut ->
                        let score = strategyContract.Contract.DuplicateScore(None)
                        score, score
                strategyContract.WithScores(scores))
        { this with StrategyContracts = strategyContracts ; Results = Some results }

type private FiveFourMajorStrategy = | SimpleStayman | ExtendedStayman | TransferOnly with
    member this.Text =
        match this with
        | SimpleStayman -> "Simple Stayman" // Stayman | 4M over 2M | 3NT over 2D
        | ExtendedStayman -> "Extended Stayman" // Stayman | 4M over 2M | 3M (5-card) over 2D, then 3NT (if doubleton) or 4M
        | TransferOnly -> "Transfer-only" // transfer | 3NT over forced 2M | correct to 4M unless doubleton

type Mode = | Minimal | DisplayOnly of interactive:bool | SaveOnly | DisplayAndSave of interactive:bool with
    member this.Display = match this with | DisplayOnly _ | DisplayAndSave _ -> true | Minimal | SaveOnly -> false
    member this.Save = match this with | SaveOnly | DisplayAndSave _ -> true | Minimal | DisplayOnly _ -> false
    member this.Interactive = match this with | DisplayOnly true | DisplayAndSave true -> true | Minimal | DisplayOnly false | SaveOnly | DisplayAndSave false -> false

let private simulationDir simulationSubDir =
    let rec findSrcDir (currentDir:DirectoryInfo) = if currentDir.Name = "src" then currentDir.FullName else findSrcDir currentDir.Parent
    let dir = Path.Combine(findSrcDir (DirectoryInfo(Environment.CurrentDirectory)), "dev-console", "simulations", simulationSubDir)
    if not (Directory.Exists dir) then Directory.CreateDirectory dir |> ignore
    dir

let private simulation<'a> i deal (strategyContracts:('a * Contract) list) =
    Some (i, Simulation<'a>.Make(deal, strategyContracts |> List.map (fun (strategy, contract) -> StrategyContract<'a>.Make(strategy, contract))))

exception MultipleContractsForOneOrMoreStrategyException
exception NoContractForOneOrMoreStrategyException

let private generateFiveFourMajorSimulation i : (int * Simulation<FiveFourMajorStrategy>) option = // 1NT opening where responder has 13-15 HCP and exactly 5-4 in the majors
    let simulation' i deal (strategyContracts:(FiveFourMajorStrategy * Contract) list) =
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
                    | 5, 4 when openerHearts >= 4 -> // always 4H (by opener) for SimpleStayman and ExtendedStayman | either 4S (by opener) or 3NT (by opener) for TransferOnly
                        let strategyContracts =
                            [
                                SimpleStayman, undoubledContract FourLevel (Suit Heart) position // 1NT | 2C | 2H | 4H | - (opener is declarer)
                                ExtendedStayman, undoubledContract FourLevel (Suit Heart) position // 1NT | 2C | 2H | 4H | - (opener is declarer)
                                if openerSpades >= 3 then
                                    TransferOnly, undoubledContract FourLevel (Suit Spade) position // 1NT | 2H | 2S | 3NT | 4S | - (opener is declarer)
                                else TransferOnly, undoubledContract ThreeLevel NoTrump position // // 1NT | 2H | 2S | 3NT | - (opener is declarer)
                            ]
                        simulation' i deal strategyContracts
                    (* Exclude deal where contract would be the same for all strategies:
                    | 5, 4 when openerSpades >= 4 -> // always 4S (by opener) for all strategies
                        let strategyContracts =
                            [
                                SimpleStayman, undoubledContract FourLevel (Suit Spade) position // 1NT | 2C | 2S | 4S | - (opener is declarer)
                                ExtendedStayman, undoubledContract FourLevel (Suit Spade) position // 1NT | 2C | 2S | 4S | - (opener is declarer)
                                TransferOnly, undoubledContract FourLevel (Suit Spade) position // 1NT | 2H | 2S | 3NT | 4S | - (opener is declarer)
                            ]
                        simulation' i deal strategyContracts *)
                    | 5, 4 -> // always 3NT (by opener) for SimpleStayman | either 4S (by declarer) or 3NT (by opener) for Extended Stayman | either 4S (by opener) or 3NT (by opener) for TransferOnly
                        let strategyContracts =
                            [
                                SimpleStayman, undoubledContract ThreeLevel NoTrump position // 1NT | 2C | 2D | 3NT | - (opener is declarer)
                                if openerSpades >= 3 then
                                    ExtendedStayman, undoubledContract FourLevel (Suit Spade) position.Partner // 1NT | 2C | 2D | 3S | 4S | - (responder is declarer)
                                else ExtendedStayman, undoubledContract ThreeLevel NoTrump position // // 1NT | 2C | 2D | 3S | 3NT | - (opener is declarer)
                                if openerSpades >= 3 then
                                    TransferOnly, undoubledContract FourLevel (Suit Spade) position // 1NT | 2H | 2S | 3NT | 4S | - (opener is declarer)
                                else TransferOnly, undoubledContract ThreeLevel NoTrump position // // 1NT | 2H | 2S | 3NT | - (opener is declarer)
                            ]
                        simulation' i deal strategyContracts
                    // Responder is 4=5...
                    (* Exclude deal where contract would be the same for all strategies:
                    | 4, 5 when openerHearts >= 4 -> // always 4H (by opener) for all strategies
                        let strategyContracts =
                            [
                                SimpleStayman, undoubledContract FourLevel (Suit Heart) position // 1NT | 2C | 2H | 4H | - (opener is declarer)
                                ExtendedStayman, undoubledContract FourLevel (Suit Heart) position // 1NT | 2C | 2H | 4H | - (opener is declarer)
                                TransferOnly, undoubledContract FourLevel (Suit Heart) position // 1NT | 2D | 2H | 3NT | 4H | - (opener is declarer)
                            ]
                        simulation' i deal strategyContracts *)
                    | 4, 5 when openerSpades >= 4 -> // always 4S (by opener) for SimpleStayman and ExtendedStayman | either 4H (by opener) or 3NT (by opener) for TransferOnly
                        let strategyContracts =
                            [
                                SimpleStayman, undoubledContract FourLevel (Suit Spade) position // 1NT | 2C | 2S | 4S | - (opener is declarer)
                                ExtendedStayman, undoubledContract FourLevel (Suit Spade) position // 1NT | 2C | 2S | 4S | - (opener is declarer)
                                if openerHearts >= 3 then
                                    TransferOnly, undoubledContract FourLevel (Suit Heart) position // 1NT | 2D | 2H | 3NT | 4H | - (opener is declarer)
                                else TransferOnly, undoubledContract ThreeLevel NoTrump position // // 1NT | 2D | 2HN | 3NT | - (opener is declarer)
                            ]
                        simulation' i deal strategyContracts
                    | 4, 5 -> // always 3NT (by opener) for SimpleStayman | either 4H (by declarer) or 3NT (by opener) for Extended Stayman | either 4H (by opener) or 3NT (by opener) for TransferOnly
                        let strategyContracts =
                            [
                                SimpleStayman, undoubledContract ThreeLevel NoTrump position // 1NT | 2C | 2D | 3NT | - (opener is declarer)
                                if openerHearts >= 3 then
                                    ExtendedStayman, undoubledContract FourLevel (Suit Heart) position.Partner // 1NT | 2C | 2D | 3H | 4H | - (responder is declarer)
                                else ExtendedStayman, undoubledContract ThreeLevel NoTrump position // // 1NT | 2C | 2D | 3H | 3NT | - (opener is declarer)
                                if openerHearts >= 3 then
                                    TransferOnly, undoubledContract FourLevel (Suit Heart) position // 1NT | 2D | 2H | 3NT | 4H | - (opener is declarer)
                                else TransferOnly, undoubledContract ThreeLevel NoTrump position // // 1NT | 2D | 2H | 3NT | - (opener is declarer)
                            ]
                        simulation' i deal strategyContracts
                    | _ -> None
                | _ -> None
            | _ -> check deal remaining
    check (Deal.MakeRandom()) [ North ; East; South ; West ]

exception NoContractForStrategyException
exception NoScoreForStrategyException

let run (mode:Mode) withDoubleDummy count =
    let simulationDescription = "Weak 1NT opening where responder has 13-15 HCP and 5-4 in the majors"
    let simulationSubDir = "Weak-1NT-when-responder-has-13-to-15-hcp-and-5-4-majors"
    let simulationDir = simulationDir simulationSubDir
    let conditionalText = if withDoubleDummy then "generating and analyzing" else "generating"
    writeNewLine $"{simulationDescription} -> {conditionalText} {count} deals:\n" ConsoleColor.Magenta
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
                    Dds.writeDoubleDummyResults results
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
            File.WriteAllText(Path.Combine(simulationDir, fileName), simulation.ToJson())
            if mode.Display then writeNewLine $"\tSaved to .../{simulationSubDir}/{fileName}\n" ConsoleColor.DarkCyan
        if mode.Interactive then
            writeNewLine "Press any key to continue..." ConsoleColor.DarkMagenta
            Console.ReadKey () |> ignore
            writeBlankLine ()
        else if not mode.Display then write "." ConsoleColor.DarkGray)
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
        let simpleStaymanMeanNotVulnerable, simpleStaymanMeanVulnerable = Mean<score>.FromList(scores SimpleStayman NotVulnerable), Mean<score>.FromList(scores SimpleStayman Vulnerable)
        let extendedStaymanMeanNotVulnerable, extendedStaymanMeanVulnerable = Mean<score>.FromList(scores ExtendedStayman NotVulnerable), Mean<score>.FromList(scores ExtendedStayman Vulnerable)
        let transferOnlyMeanNotVulnerable, transferOnlyMeanVulnerable = Mean<score>.FromList(scores TransferOnly NotVulnerable), Mean<score>.FromList(scores TransferOnly Vulnerable)
        if not mode.Display then writeBlankLine ()
        writeNewLine "\tMean scores per strategy:\n" ConsoleColor.DarkGray
        writeNewLine $"\t{SimpleStayman.Text} -> {simpleStaymanMeanNotVulnerable.Mean} (non-vul.) | {simpleStaymanMeanVulnerable.Mean} (vul.)" ConsoleColor.Gray
        writeNewLine $"\t{ExtendedStayman.Text} -> {extendedStaymanMeanNotVulnerable.Mean} (non-vul.) | {extendedStaymanMeanVulnerable.Mean} (vul.)" ConsoleColor.Gray
        writeNewLine $"\t{TransferOnly.Text} -> {transferOnlyMeanNotVulnerable.Mean} (non-vul.) | {transferOnlyMeanVulnerable.Mean} (vul.)" ConsoleColor.Gray
        writeBlankLine ()
    let conditionalText = if withDoubleDummy then "generated and analyzed" else "generated"
    if mode.Display then writeNewLine $"{simulationDescription} -> {conditionalText} {count} matching deal/s (from {iMax} random deal/s)" ConsoleColor.DarkYellow
    else writeNewLine $"{simulationDescription} -> {conditionalText} {count} matching deal/s (from {iMax} random deal/s) in {(DateTime.UtcNow - start).TotalSeconds} seconds" ConsoleColor.DarkYellow
