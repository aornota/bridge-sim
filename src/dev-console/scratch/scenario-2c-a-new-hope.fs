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

type PartnerShapeType = | BalancedP | OneSuiterP | OtherP
    with
    static member Make = function
        | FourThreeThreeThree | FourFourThreeTwo | FiveThreeThreeTwo -> BalancedP
        | FourFourFourOne | FiveFourFourZero | FiveFourTwoTwo | FiveFourThreeOne | FiveFiveTwoOne | FiveFiveThreeZero | SixFourTwoOne | SixFourThreeZero | SixFiveOneOne | SixFiveTwoZero | SixSixOneZero | SevenFiveOneZero | SevenSixZeroZero | EightFiveZeroZero -> OtherP
        // TODO-NMB: Treat 63nn as OtherP if weak suit?...
        | _ -> OneSuiterP
    member this.Text = match this with | BalancedP -> "Balanced" | OneSuiterP -> "One-suiter" | OtherP -> "Other"

let private percentageOf ofTotal value = $"%0.2f{float (100 * value) / float ofTotal}%%"

let allStatistics count =
    if count = 0 then raise CountMustBeGreaterThanZeroException
    writeNewLine $"Statistics for all hands (count = %i{count}):\n" ConsoleColor.Magenta
    let mutable total = 0
    let generate _ =
        total <- total + 1
        if total % 1000 = 0 then write "." ConsoleColor.DarkMagenta
        (Deal.MakeRandom ()).Hand South
    let results = Seq.initInfinite generate |> Seq.take count |> List.ofSeq // force evaluation
    writeBlankLine ()
    results
    |> List.groupBy (fun hand -> OpenerShapeType.Make hand.Shape)
    |> List.sortBy fst
    |> List.iter (fun (shapeType, elements) -> writeNewLine $"%s{shapeType.Text} -> %s{elements.Length |> percentageOf count}" ConsoleColor.Cyan)

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
        if openerStrategy.Qualifies hand then Some (deal, hand, deal.Hand North)
        else None
    let results = Seq.initInfinite generate |> Seq.choose id |> Seq.take count |> List.ofSeq // force evaluation
    writeNewLine $"\nO ~ Percentage of total: %s{count |> percentageOf total}\n" ConsoleColor.Cyan
    results
    |> List.groupBy (fun (_, hand, _) -> OpenerShapeType.Make hand.Shape)
    |> List.sortBy fst
    |> List.iter (fun (shapeType, elements) ->
        let countForShapeType = elements.Length
        writeNewLine $"O ~ %s{shapeType.Text} -> %s{countForShapeType |> percentageOf count}" ConsoleColor.Cyan
        let resultsForShapeType =
            elements
            |> List.groupBy (fun (_, hand, partnerHand) -> PartnerShapeType.Make partnerHand.Shape, partnerHand.Cc)
            |> List.sortBy fst
            |> List.map (fun (cc, elements) -> cc, elements.Length)
        let equalsFilter value = fun valueToCheck -> valueToCheck = value
        let countForShapeTypeFilter shapeTypeFilter ccFilter =
            let filteredForShapeType = resultsForShapeType |> List.filter (fun (grouping, _) -> shapeTypeFilter (fst grouping))
            match ccFilter with
            | Some ccFilter -> filteredForShapeType |> List.filter (fun (grouping, _) -> ccFilter (snd grouping)) |> List.sumBy snd
            | None -> filteredForShapeType |> List.sumBy snd
        let balancedFilter, oneSuiterFilter, otherFilter = equalsFilter BalancedP, equalsFilter OneSuiterP, equalsFilter OtherP
        let negativeFilter = equalsFilter 0<cc>
        let balancedCount = countForShapeTypeFilter balancedFilter None
        let balancedCountNegative = countForShapeTypeFilter balancedFilter (Some negativeFilter)
        let balancedCountPositive = balancedCount - balancedCountNegative
        let oneSuiterCount = countForShapeTypeFilter oneSuiterFilter None
        let oneSuiterCountNegative = countForShapeTypeFilter oneSuiterFilter (Some negativeFilter)
        let oneSuiterCountPositive = oneSuiterCount - oneSuiterCountNegative
        let otherCount = countForShapeTypeFilter otherFilter None
        let otherCountNegative = countForShapeTypeFilter otherFilter (Some negativeFilter)
        let otherCountPositive = otherCount - otherCountNegative
        writeBlankLine ()
        writeNewLine $"\tR ~ {BalancedP.Text} -> %s{balancedCount |> percentageOf countForShapeType}" ConsoleColor.DarkCyan
        writeNewLine $"\t\tNegative -> %s{balancedCountNegative |> percentageOf countForShapeType} (%s{balancedCountNegative |> percentageOf balancedCount})" ConsoleColor.DarkCyan
        writeNewLine $"\t\tPositive -> %s{balancedCountPositive |> percentageOf countForShapeType} (%s{balancedCountPositive |> percentageOf balancedCount})" ConsoleColor.DarkCyan
        writeNewLine $"\tR ~ {OneSuiterP.Text} -> %s{oneSuiterCount |> percentageOf countForShapeType}" ConsoleColor.DarkCyan
        writeNewLine $"\t\tNegative -> %s{oneSuiterCountNegative |> percentageOf countForShapeType} (%s{oneSuiterCountNegative |> percentageOf oneSuiterCount})" ConsoleColor.DarkCyan
        writeNewLine $"\t\tPositive -> %s{oneSuiterCountPositive |> percentageOf countForShapeType} (%s{oneSuiterCountPositive |> percentageOf oneSuiterCount})" ConsoleColor.DarkCyan
        writeNewLine $"\tR ~ {OtherP.Text} -> %s{otherCount |> percentageOf countForShapeType}" ConsoleColor.DarkCyan
        writeNewLine $"\t\tNegative -> %s{otherCountNegative |> percentageOf countForShapeType} (%s{otherCountNegative |> percentageOf otherCount})" ConsoleColor.DarkCyan
        writeNewLine $"\t\tPositive -> %s{otherCountPositive |> percentageOf countForShapeType} (%s{otherCountPositive |> percentageOf otherCount})" ConsoleColor.DarkCyan
        let countForCcFilter ccFilter = resultsForShapeType |> List.filter (fun (grouping, _) -> ccFilter (snd grouping)) |> List.sumBy snd
        let negativeCount = countForCcFilter negativeFilter
        let positiveCount = countForShapeType - negativeCount
        let cc1Count, cc2Count, cc3Count, cc4PlusCount = countForCcFilter (equalsFilter 1<cc>), countForCcFilter (equalsFilter 2<cc>), countForCcFilter (equalsFilter 3<cc>), countForCcFilter (fun cc -> cc >= 4<cc>)
        writeBlankLine ()
        writeNewLine $"\tR ~ Negative -> %s{negativeCount |> percentageOf countForShapeType}" ConsoleColor.DarkCyan
        writeNewLine $"\tR ~ Positive -> %s{positiveCount |> percentageOf countForShapeType}" ConsoleColor.DarkCyan
        writeNewLine $"\t\t1 CC -> %s{cc1Count |> percentageOf countForShapeType} (%s{cc1Count |> percentageOf positiveCount})" ConsoleColor.DarkCyan
        writeNewLine $"\t\t2 CC -> %s{cc2Count |> percentageOf countForShapeType} (%s{cc2Count |> percentageOf positiveCount})" ConsoleColor.DarkCyan
        writeNewLine $"\t\t3 CC -> %s{cc3Count |> percentageOf countForShapeType} (%s{cc3Count |> percentageOf positiveCount})" ConsoleColor.DarkCyan
        writeNewLine $"\t\t4+ CC -> %s{cc4PlusCount |> percentageOf countForShapeType} (%s{cc4PlusCount |> percentageOf positiveCount})" ConsoleColor.DarkCyan
        writeBlankLine ()
    )

