module Aornota.BridgeSim.DevConsole.Scratch.Scenario.TwoCANewHope

open Aornota.BridgeSim.Common.Console
open Aornota.BridgeSim.Dds.Interop.Dds
open Aornota.BridgeSim.DevConsole.Common
open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Evaluation.Core
open Aornota.BridgeSim.Domain.Random.Deal

open System

// TODO-NMB: More strategies?...
type OpenerStrategy = | Basic
    with
    member this.Qualifies (hand:Hand) =
        match this with
        | Basic ->
            match hand.ShapeCategory = Balanced, hand.Hcp, hand.Ltc with
            | true, hcp, _ when hcp >= 22<hcp> -> true // balanced with 22+ HCP
            | false, _, ltc when ltc <= 4 -> true // not balanced with 4- LTC
            | _ -> false
    member this.Text = match this with | Basic -> "22+ HCP balanced or LTC <= 4"

type OpenerShapeType = | BalancedO | ThreeSuiterO | TwoSuiterO | OneSuiterO
    with
    static member Make = function
        | FourThreeThreeThree | FourFourThreeTwo | FiveThreeThreeTwo -> BalancedO
        | FourFourFourOne | FiveFourFourZero -> ThreeSuiterO
        | FiveFourTwoTwo | FiveFourThreeOne | FiveFiveTwoOne | FiveFiveThreeZero | SixFourTwoOne | SixFourThreeZero | SixFiveOneOne | SixFiveTwoZero | SixSixOneZero | SevenFiveOneZero | SevenSixZeroZero | EightFiveZeroZero -> TwoSuiterO
        | _ -> OneSuiterO
    member this.Text = match this with | BalancedO -> "Balanced" | ThreeSuiterO -> "Three-suiter" | TwoSuiterO -> "Two-suiter" | OneSuiterO -> "One-suiter"

(*
type PartnerShapeType = | BalancedP | OneSuiterP | OtherP

type PartnerStrength = | CC0 | CC1or2 | CC3 | CC4 | CC5Plus
*)

let allStatistics count =
    if count = 0 then raise CountMustBeGreaterThanZeroException
    writeNewLine $"Statistics for all hands (count = %i{count}):\n" ConsoleColor.Magenta
    let mutable total = 0
    let generate _ =
        total <- total + 1
        if total % 1000 = 0 then write "." ConsoleColor.DarkMagenta
        let deal = Deal.MakeRandom ()
        deal, deal.Hand South
    let results =
        Seq.initInfinite generate |> Seq.take count
        |> Seq.groupBy (fun (_, hand) -> OpenerShapeType.Make hand.Shape)
        |> Seq.map (fun (shapeType, values) -> shapeType, values |> Seq.length)
        |> Seq.sortBy fst
        |> List.ofSeq // force evaluation
    writeBlankLine ()
    results
    |> List.iter (fun (shapeType, countForShapeType) ->
        let percentageOfCount = float (100 * countForShapeType) / float count
        writeNewLine $"%s{shapeType.Text} -> %0.2f{percentageOfCount}%%" ConsoleColor.Cyan)

let forcingOpening2CStatistics (openerStrategy:OpenerStrategy) count =
    if count = 0 then raise CountMustBeGreaterThanZeroException
    writeNewLine $"Statistics for 2C forcing opening hands ({openerStrategy.Text} | count = %i{count}):\n" ConsoleColor.Magenta
    let mutable total = 0
    (* TODO-NMB: For double-dummy, don't restrict by position?...
    let generate _ =
        let rec check (deal:Deal) positions =
            match positions with
            | [] -> None
            | position :: remaining ->
                let hand = deal.Hand(position)
                match openerStrategy with
                | Some strategy ->
                    if strategy.Qualifies hand then Some (deal, hand)
                    else check deal remaining
                | None -> Some (deal, hand)
        total <- total + 1
        check (Deal.MakeRandom()) [ North ; East; South ; West ]
    *)
    let generate _ =
        total <- total + 1
        if total % 5000 = 0 then write "." ConsoleColor.DarkMagenta
        let deal = Deal.MakeRandom ()
        let hand = deal.Hand South
        if openerStrategy.Qualifies hand then Some (deal, hand)
        else None
    let results =
        Seq.initInfinite generate |> Seq.choose id |> Seq.take count
        |> Seq.groupBy (fun (_, hand) -> OpenerShapeType.Make hand.Shape)
        // TODO-NMB: Extend analysis...
        |> Seq.map (fun (shapeType, values) ->
            shapeType, values |> Seq.length
        )
        |> Seq.sortBy fst
        |> List.ofSeq // force evaluation
    let percentageOfTotal = float (100 * count) / float total
    writeNewLine $"\nPercentage of total: %0.2f{percentageOfTotal}%%\n" ConsoleColor.Cyan
    results
    |> List.iter (fun (shapeType, countForShapeType) ->
        let percentageOfCount = float (100 * countForShapeType) / float count
        writeNewLine $"%s{shapeType.Text} -> %0.2f{percentageOfCount}%%" ConsoleColor.Cyan
    )

