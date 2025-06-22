module Aornota.BridgeSim.DevConsole.Scratch.Scenario.TwoCANewHope

open Aornota.BridgeSim.Common.Console
open Aornota.BridgeSim.Dds.Interop.Dds
open Aornota.BridgeSim.DevConsole.Common
open Aornota.BridgeSim.DevConsole.Scratch.Dds
open Aornota.BridgeSim.Domain.Auction
open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Evaluation.Core
open Aornota.BridgeSim.Domain.Formatting.Deal
open Aornota.BridgeSim.Domain.Random.Deal

open System

type OpenerStrategy = | Basic | Advanced | AdvancedRedux
    with
    member this.Qualifies (hand:Hand) =
        match this with
        | Basic ->
            if hand.ShapeCategory = Balanced then hand.Hcp >= 22<hcp> // balanced with 22+ HCP
            else hand.Ltc <= 4 // not balanced with LTC <= 4
        | Advanced ->
            if hand.ShapeCategory = Balanced then hand.Hcp >= 22<hcp> // balanced with 22+ HCP
            else
                let spadesCount, heartsCount, _, _ = hand.SuitCounts
                match hand.Shape, spadesCount >= 5 || heartsCount >= 5 with
                | FourFourFourOne, _ | FiveFourFourZero, _ -> hand.Ltc <= 3 // three-suiter with LTC <= 3
                | _, true -> hand.Ltc <= 4 // one-/two-suiter with 5+ major and LTC <= 4
                | _, false -> hand.Ltc <= 3 // one-/two-suiter without 5+ major and LTC <= 3
        | AdvancedRedux ->
            if hand.ShapeCategory = Balanced then hand.Hcp >= 22<hcp> // balanced with 22+ HCP
            else
                let spadesCount, heartsCount, _, _ = hand.SuitCounts
                // TODO-NMB: Adjust these (e.g. based on overall frequencies)?...
                match hand.Shape, spadesCount >= 5 || heartsCount >= 5 with
                | FourFourFourOne, _ | FiveFourFourZero, _ -> hand.AdjustedLtc <= 4.25m // three-suiter with ALTC <= 4.25
                | _, true -> hand.AdjustedLtc <= 5m // one-/two-suiter with 5+ major and ALTC <= 5
                | _, false -> hand.AdjustedLtc <= 4.25m // one-/two-suiter without 5+ major and ALTC <= 4.25
    member this.Text =
        match this with
        | Basic -> "22+ HCP balanced or LTC <= 4"
        | Advanced -> "22+ HCP balanced or LTC <= 4 for one-/two-suiter with 5+ major or LTC <= 3 otherwise (including three-suiter with 5+ major)"
        | AdvancedRedux -> "22+ HCP balanced or ALTC <= 5 for one-/two-suiter with 5+ major or ALTC <= 4.25 otherwise (including three-suiter with 5+ major)"

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
    static member Make (hand:Hand) =
        match hand.Shape with
        | FourThreeThreeThree | FourFourThreeTwo | FiveThreeThreeTwo -> BalancedP
        | FourFourFourOne | FiveFourFourZero | FiveFourTwoTwo | FiveFourThreeOne | FiveFiveTwoOne | FiveFiveThreeZero | SixFourTwoOne | SixFourThreeZero | SixFiveOneOne | SixFiveTwoZero | SixSixOneZero | SevenFiveOneZero | SevenSixZeroZero | EightFiveZeroZero -> OtherP
        // TODO-NMB: Treat 63nn as OtherP if weak suit (e.g. not if at leeast one of AKQ and at least two of AKQJT)?...
        | SixThreeTwoTwo | SixThreeThreeOne -> OtherP
        | _ -> OneSuiterP
    member this.Text = match this with | BalancedP -> "Balanced" | OneSuiterP -> "One-suiter" | OtherP -> "Other"

type PartnerResponse = | Positive of cc : int<cc> | Negative
    with
    static member Make (hand:Hand) =
        // Define a positive hand as having an ace / a king / at least two queens / one queen and at least three jacks / one queen and two jacks (one of which is in the same suit as the queen).
        if hand.Cc >= 1<cc> then Positive hand.Cc
        else
            let suitsForRank rank = hand.Cards |> List.choose (fun card -> if card.Rank = rank then Some card.Suit else None)
            let queensSuits, jacksSuits = suitsForRank Queen, suitsForRank Jack
            match queensSuits.Length, jacksSuits.Length, queensSuits with
            | queens, _, _ when queens >= 2 -> Positive 0<cc>
            | queens, jacks, _ when queens = 1 && jacks >= 3 -> Positive 0<cc>
            | queens, jacks, queenSuit :: _ when queens = 1 && jacks = 2 && jacksSuits |> List.contains queenSuit -> Positive 0<cc>
            | _ -> Negative
    member this.Cc = match this with | Positive cc -> cc | Negative -> 0<cc>

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

let forcingOpening2CAnalysis (openerStrategy:OpenerStrategy) count =
    if count = 0 then raise CountMustBeGreaterThanZeroException
    writeNewLine $"Analysis for 2C forcing opening hands ({openerStrategy.Text} | partner would pass 1-level opening | count = %i{count}):\n" ConsoleColor.Magenta
    let mutable total = 0
    let generate _ =
        let rec check (deal:Deal) positions =
            match positions with
            | [] -> None
            | position :: remaining ->
                let hand = deal.Hand(position)
                if openerStrategy.Qualifies hand then
                    let partnerHand = deal.Hand(position.Partner)
                    let partnerShape = partnerHand.Shape
                    // TODO-NMB: Exclude cases where partner might respond (e.g. pre-emptive raise / weak jump response)?...
                    match partnerHand.Hcp, partnerShape.MaxAny, partnerShape.MinAny with
                    | hcp, _, _ when hcp <= 3<hcp> -> Some (deal, position)
                    | hcp, maxAny, minAny when hcp = 4<hcp> && not (maxAny >= 5 && minAny <= 1)-> Some (deal, position)
                    | hcp, maxAny, minAny when hcp = 5<hcp> && not (maxAny >= 5 || minAny <= 1) -> Some (deal, position)
                    | _ -> check deal remaining
                else check deal remaining
        total <- total + 1
        if total % 1000 = 0 then write "." ConsoleColor.DarkMagenta
        check (Deal.MakeRandom ()) [ North ; East; South ; West ]
    let results =
        Seq.initInfinite generate
        |> Seq.choose id
        |> Seq.take count
        |> Seq.map (fun (deal, position) ->
            let games = (calculateDoubleDummy deal).Games(Partnership.ForPosition position)
            let canMakeGame, canMakeNoTrumpGame = games.Length > 0, games |> List.contains NoTrump
            let openerHand = deal.Hand position
            let openerSpades, openerHearts, _, _ = openerHand.SuitCounts
            let has5PlusMajor = openerSpades >= 5 || openerHearts >= 5
            (OpenerShapeType.Make openerHand.Shape, has5PlusMajor), canMakeGame, canMakeNoTrumpGame, PartnerResponse.Make (deal.Hand position.Partner)
        )
        |> List.ofSeq // force evaluation
    results
    |> List.groupBy (fun (shapeTypeEtc, _, _, _) -> shapeTypeEtc)
        |> List.sortBy (fun ((shapeType, has5PlusMajor), _) -> shapeType, not has5PlusMajor)
        |> List.iter (fun ((shapeType, has5PlusMajor), elements) ->
        let countForShapeTypeEtc = elements.Length
        let extraText = if has5PlusMajor then "with 5+ major" else "without 5+ major"
        writeNewLine $"O ~ %s{shapeType.Text} {extraText} -> %s{countForShapeTypeEtc |> percentageOf count}" ConsoleColor.Cyan
        let canMakeGameCount = elements |> List.filter (fun (_, canMakeGame, _, _) -> canMakeGame) |> List.length
        let canMakeGameNegativeCount = elements |> List.filter (fun (_, canMakeGame, _, partnerResponse) -> canMakeGame && partnerResponse.IsNegative) |> List.length
        let canMakeGameCC0Count = elements |> List.filter (fun (_, canMakeGame, _, partnerResponse) -> canMakeGame && partnerResponse.Cc = 0<cc>) |> List.length
        let canMakeNoTrumpGameCount = elements |> List.filter (fun (_, _, canMakeNoTrumpGame, _) -> canMakeNoTrumpGame) |> List.length
        let canMakeNoTrumpGameNegativeCount = elements |> List.filter (fun (_, _, canMakeNoTrumpGame, partnerResponse) -> canMakeNoTrumpGame && partnerResponse.IsNegative) |> List.length
        let canMakeNoTrumpGameCC0Count = elements |> List.filter (fun (_, _, canMakeNoTrumpGame, partnerResponse) -> canMakeNoTrumpGame && partnerResponse.Cc = 0<cc>) |> List.length
        writeBlankLine ()
        writeNewLine $"\tCan make game -> %s{canMakeGameCount |> percentageOf countForShapeTypeEtc}" ConsoleColor.DarkCyan
        writeNewLine $"\tCan make game when partner negative -> %s{canMakeGameNegativeCount |> percentageOf countForShapeTypeEtc}" ConsoleColor.DarkCyan
        writeNewLine $"\tCan make game only when partner positive -> %s{canMakeGameCount - canMakeGameNegativeCount |> percentageOf countForShapeTypeEtc}" ConsoleColor.DarkCyan
        writeNewLine $"\tCan make game when partner 0 CC -> %s{canMakeGameCC0Count |> percentageOf countForShapeTypeEtc}" ConsoleColor.DarkCyan
        writeNewLine $"\tCan make game only when partner 1+ CC -> %s{canMakeGameCount - canMakeGameCC0Count |> percentageOf countForShapeTypeEtc}" ConsoleColor.DarkCyan
        writeNewLine $"\tCan make NT game -> %s{canMakeNoTrumpGameCount |> percentageOf countForShapeTypeEtc}" ConsoleColor.DarkCyan
        writeNewLine $"\tCan make NT game when partner negative -> %s{canMakeNoTrumpGameNegativeCount |> percentageOf countForShapeTypeEtc}" ConsoleColor.DarkCyan
        writeNewLine $"\tCan make NT game only when partner positive -> %s{canMakeNoTrumpGameCount - canMakeNoTrumpGameNegativeCount |> percentageOf countForShapeTypeEtc}" ConsoleColor.DarkCyan
        writeNewLine $"\tCan make NT game when partner 0 CC -> %s{canMakeNoTrumpGameCC0Count |> percentageOf countForShapeTypeEtc}" ConsoleColor.DarkCyan
        writeNewLine $"\tCan make NT game only when partner 1+ CC -> %s{canMakeNoTrumpGameCount - canMakeNoTrumpGameCC0Count |> percentageOf countForShapeTypeEtc}" ConsoleColor.DarkCyan
        writeBlankLine ()
    )

let forcingOpening2CStatistics (openerStrategy:OpenerStrategy) count =
    if count = 0 then raise CountMustBeGreaterThanZeroException
    writeNewLine $"Statistics for 2C forcing opening hands ({openerStrategy.Text} | count = %i{count}):\n" ConsoleColor.Magenta
    let mutable total = 0
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
        // TODO-NMB: Redefine CC / positive / negative (cf. forcingOpening2CAnalysis)?...
        let resultsForShapeType =
            elements
            |> List.groupBy (fun (_, hand, partnerHand) -> PartnerShapeType.Make partnerHand, partnerHand.Cc)
            |> List.sortBy fst
            |> List.map (fun (cc, elements) -> cc, elements.Length)
        let equalsFilter value = fun valueToCheck -> valueToCheck = value
        let countForShapeTypeFilter shapeTypeFilter ccFilter =
            let filteredForShapeType = resultsForShapeType |> List.filter (fun (grouping, _) -> shapeTypeFilter (fst grouping))
            match ccFilter with
            | Some ccFilter -> filteredForShapeType |> List.filter (fun (grouping, _) -> ccFilter (snd grouping)) |> List.sumBy snd
            | None -> filteredForShapeType |> List.sumBy snd
        let balancedFilter, oneSuiterFilter, otherFilter, negativeFilter = equalsFilter BalancedP, equalsFilter OneSuiterP, equalsFilter OtherP, equalsFilter 0<cc>
        let balancedCount, balancedCountNegative = countForShapeTypeFilter balancedFilter None, countForShapeTypeFilter balancedFilter (Some negativeFilter)
        let balancedCountPositive = balancedCount - balancedCountNegative
        let oneSuiterCount, oneSuiterCountNegative = countForShapeTypeFilter oneSuiterFilter None, countForShapeTypeFilter oneSuiterFilter (Some negativeFilter)
        let oneSuiterCountPositive = oneSuiterCount - oneSuiterCountNegative
        let otherCount, otherCountNegative = countForShapeTypeFilter otherFilter None, countForShapeTypeFilter otherFilter (Some negativeFilter)
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

