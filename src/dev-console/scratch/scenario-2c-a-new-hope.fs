module Aornota.BridgeSim.DevConsole.Scratch.Scenario.TwoCANewHope

open Aornota.BridgeSim.Common.Console
open Aornota.BridgeSim.Dds.Interop.Dds
open Aornota.BridgeSim.DevConsole.Common
open Aornota.BridgeSim.Domain.Auction
open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Evaluation.Core
open Aornota.BridgeSim.Domain.Formatting.Auction
open Aornota.BridgeSim.Domain.Formatting.Core
open Aornota.BridgeSim.Domain.Formatting.Deal
open Aornota.BridgeSim.Domain.Random.Deal

open System

let forcingOpening2CStatistics count =
    let addOrIncrement (key: 'a) (map: Map<'a, int>) =
        match map |> Map.tryFind key with
        | Some value -> map |> Map.add key (value + 1)
        | None -> map |> Map.add key 1
    if count = 0 then raise CountMustBeGreaterThanZeroException
    writeNewLine (sprintf "Statistics for 2C forcing opening hands (count = %i):\n" count) ConsoleColor.Magenta
    let mutable total = 0
    let generate _ =
        let rec check (deal:Deal) positions =
            match positions with
            | [] -> None
            | position :: remaining ->
                let hand = deal.Hand(position)
                match hand.ShapeCategory = Balanced, hand.Hcp, hand.Ltc with
                | true, hcp, _ when hcp >= 22<hcp> -> Some (deal, hand) // balanced with 22+ HCP
                | false, _, ltc when ltc <= 4 -> Some (deal, hand) // not balanced with 4- LTC
                | _ -> check deal remaining
        total <- total + 1
        // TODO-NMB: Only for single seat?...
        //check (Deal.MakeRandom()) [ North ; East; South ; West ]
        check (Deal.MakeRandom()) [ South ]
    let generator = Seq.initInfinite generate |> Seq.choose id
    let mutable map = Map.empty<ShapeCategory * Shape, int>
    generator |> Seq.take count |> Seq.iteri (fun i (_, hand) ->
        map <- map |> addOrIncrement (hand.ShapeCategory, hand.Shape)
    )
    let percentageOfTotal = float (100 * count) / float total
    writeNewLine $"Percentage of total: %0.2f{percentageOfTotal}%%\n" ConsoleColor.Cyan
    map |> Map.iter (fun (category, shape) value ->
        let percentageOfCount = float (100 * value) / float count
        writeNewLine $"%s{category.TextUpper} %s{shape.Text} -> %i{value} (%0.2f{percentageOfCount}%%)" ConsoleColor.Cyan
    )

