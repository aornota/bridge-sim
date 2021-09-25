module Aornota.BridgeSim.DevConsole.Scratch.Simulation

open Aornota.BridgeSim.Dds.Interop.Dds
open Aornota.BridgeSim.Domain.Auction
open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Scoring.Auction

open System
open System.IO
open Thoth.Json.Net

exception CannotDeserializeSimulationException of json:string * error:string
exception MultipleContractsForOneOrMoreStrategyException
exception NoContractForOneOrMoreStrategyException
exception NoContractForStrategyException
exception NoScoreForStrategyException

type StrategyContract<'a> = { Strategy : 'a ; Contract : Contract ; Scores : (int<score> * int<score>) option } with
    static member Make(strategy, contract) = { Strategy = strategy ; Contract = contract ; Scores = None }
    member this.WithScores(scores) = { this with Scores = Some scores }

type Simulation<'a> = { Deal : Deal ; StrategyContracts : StrategyContract<'a> list ; Results : DoubleDummyResults option } with
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

type Mode = | Minimal | DisplayOnly of interactive:bool | SaveOnly | DisplayAndSave of interactive:bool with
    member this.Display = match this with | DisplayOnly _ | DisplayAndSave _ -> true | Minimal | SaveOnly -> false
    member this.Save = match this with | SaveOnly | DisplayAndSave _ -> true | Minimal | DisplayOnly _ -> false
    member this.Interactive = match this with | DisplayOnly true | DisplayAndSave true -> true | Minimal | DisplayOnly false | SaveOnly | DisplayAndSave false -> false

let scenarioDir subDir =
    let rec findSrcDir (currentDir:DirectoryInfo) = if currentDir.Name = "src" then currentDir.FullName else findSrcDir currentDir.Parent
    let dir = Path.Combine(findSrcDir (DirectoryInfo(Environment.CurrentDirectory)), "dev-console", "simulations", subDir)
    if not (Directory.Exists dir) then Directory.CreateDirectory dir |> ignore
    dir

let simulation<'a> (i:int) deal (strategyContracts:('a * Contract) list) =
    Some (i, Simulation<'a>.Make(deal, strategyContracts |> List.map (fun (strategy, contract) -> StrategyContract<'a>.Make(strategy, contract))))
