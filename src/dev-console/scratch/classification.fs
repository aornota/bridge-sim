module Aornota.BridgeSim.DevConsole.Scratch.Classification

open Aornota.BridgeSim.Common.Console
open Aornota.BridgeSim.DevConsole.Common
open Aornota.BridgeSim.Domain.Auction
open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Evaluation.Core
open Aornota.BridgeSim.Domain.Formatting.Auction
open Aornota.BridgeSim.Domain.Formatting.Core
open Aornota.BridgeSim.Domain.Random.Deal

open System

(* TODO-NMB?:
     - Type for classification e.g. OneSuiter[Major|Minor] | TwoSuiter[Majors|Minors|EqualOrLongerMajor|LongerMinor] | ThreeSuiter[ShortMajor|ShortMinor] | Balanced...
     - Type for balanced HCP range, e.g. 0-5 | 6-9 | 10-11 | 12-14 | 15-17 | 18-19 | 20-22 | 23-24 | 25+...
     - Type for not-balanced HCP range, e.g. 0-5 | 6-10 | 11-12 | 13-15 | 16-19 | 20-22 | 23+...
     - Type for combined HCP range?...
     - Type for level-of-fit, e.g. 7-only | single-8 | single-9 | single-10+ | double-8 | double-9 | double-10+ | triple-8...
     - Type for Hand + partner Hand?...

     - Using random deals:
        -- Get all 4 sets of Hand + partner Hand from each deal...
        -- List.collect these...
        -- List.take desired number of hands...
        -- Classify...
        -- Output summary statistics, e.g. classification broken down by HCP range (or by combined HCP range? level-of-fit? &c.)... *)

type Strength = ElevenToFifteen | SixteenToNineteen with
    member this.Text = match this with | ElevenToFifteen -> "11-15 HCP" | SixteenToNineteen -> "16-19 HCP"

type OpenedMajorLength = ExactlyFourCards | FiveOrMoreCards with
    member this.Text(suit:Suit) = match this with | ExactlyFourCards -> $"exactly 4 {suit.TextLowerPlural}" | FiveOrMoreCards -> $"5+ {suit.TextLowerPlural}"

type OneClubOpening =
    | LongClubs of Strength
    | LongDiamonds of Strength
    | ClubsAndDiamonds of Strength
    | BalancedNineteenToTwentyOne
    with
    member this.Text =
        match this with
        | LongClubs strength -> $"6+ {Club.TextLowerPlural} with {strength.Text}"
        | LongDiamonds strength -> $"6+ {Diamond.TextLowerPlural} with {strength.Text}"
        | ClubsAndDiamonds strength -> $"{Club.TextLowerPlural} and {Diamond.TextLowerPlural} with {strength.Text}"
        | BalancedNineteenToTwentyOne -> "balanced with 19-21 HCP"

type OneDiamondOpening = | BalancedTenToTwelve | BalancedSixteenToEighteen with
    member this.Text = match this with | BalancedTenToTwelve -> "balanced with 10-12 HCP" | BalancedSixteenToEighteen -> "balanced with 16-19 HCP"

type OneHeartOpening =
    | LongHearts of Strength
    | HeartsAndSpades of Strength * OpenedMajorLength
    | HeartsAndDiamonds of Strength * OpenedMajorLength
    | HeartsAndClubs of Strength * OpenedMajorLength
    | HeartsSpadesAndDiamonds of Strength * OpenedMajorLength
    | HeartsSpadesAndClubs of Strength * OpenedMajorLength
    | HeartsDiamondsAndClubs of Strength * OpenedMajorLength
    | FiveHeartsThreeThreeTwo of Strength
    with
    member this.Text =
        match this with
        | LongHearts strength -> $"6+ {Heart.TextLowerPlural} with {strength.Text}"
        | HeartsAndSpades (strength, openedMajorLength) -> $"{Heart.TextLowerPlural} and {Spade.TextLowerPlural} with {strength.Text} and {openedMajorLength.Text(Heart)}"
        | HeartsAndDiamonds (strength, openedMajorLength) -> $"{Heart.TextLowerPlural} and {Diamond.TextLowerPlural} with {strength.Text} and {openedMajorLength.Text(Heart)}"
        | HeartsAndClubs (strength, openedMajorLength) -> $"{Heart.TextLowerPlural} and {Club.TextLowerPlural} with {strength.Text} and {openedMajorLength.Text(Heart)}"
        | HeartsSpadesAndDiamonds (strength, openedMajorLength) -> $"{Heart.TextLowerPlural}. {Spade.TextLowerPlural} and {Diamond.TextLowerPlural} (4441 or 5440) with {strength.Text} and {openedMajorLength.Text(Heart)}"
        | HeartsSpadesAndClubs (strength, openedMajorLength) -> $"{Heart.TextLowerPlural}. {Spade.TextLowerPlural} and {Club.TextLowerPlural} (4441 or 5440) with {strength.Text} and {openedMajorLength.Text(Heart)}"
        | HeartsDiamondsAndClubs (strength, openedMajorLength) -> $"{Heart.TextLowerPlural}. {Diamond.TextLowerPlural} and {Club.TextLowerPlural} (4441 or 5440) with {strength.Text} and {openedMajorLength.Text(Heart)}"
        | FiveHeartsThreeThreeTwo strength -> $"5332 (exactly 5 {Heart.TextLowerPlural}) with {strength.Text}"

type OneSpadeOpening =
    | LongSpades of Strength
    | SpadesAndDiamonds of Strength * OpenedMajorLength
    | SpadesAndClubs of Strength * OpenedMajorLength
    | SpadesDiamondsAndClubs of Strength * OpenedMajorLength
    | FiveSpadesThreeThreeTwo of Strength
    with
    member this.Text =
        match this with
        | LongSpades strength -> $"6+ {Spade.TextLowerPlural} with {strength.Text}"
        | SpadesAndDiamonds (strength, openedMajorLength) -> $"{Spade.TextLowerPlural} and {Diamond.TextLowerPlural} with {strength.Text} and {openedMajorLength.Text(Spade)}"
        | SpadesAndClubs (strength, openedMajorLength) -> $"{Spade.TextLowerPlural} and {Club.TextLowerPlural} with {strength.Text} and {openedMajorLength.Text(Spade)}"
        | SpadesDiamondsAndClubs (strength, openedMajorLength) -> $"{Spade.TextLowerPlural}. {Diamond.TextLowerPlural} and {Club.TextLowerPlural} (4441 or 5440) with {strength.Text} and {openedMajorLength.Text(Spade)}"
        | FiveSpadesThreeThreeTwo strength -> $"5332 (exactly 5 {Spade.TextLowerPlural}) with {strength.Text}"

type OneNoTrumpOpening = | BalancedThirteenToFifteen with
    member this.Text = match this with | BalancedThirteenToFifteen -> "balanced with 13-15 HCP"

type OneLevelOpening = | OneClub of OneClubOpening | OneDiamond of OneDiamondOpening | OneHeart of OneHeartOpening | OneSpade of OneSpadeOpening | OneNoTrump of OneNoTrumpOpening with
    member this.Text =
        match this with
        | OneClub oneClubOpening -> oneClubOpening.Text
        | OneDiamond oneDiamondOpening -> oneDiamondOpening.Text
        | OneHeart oneHeartOpening -> oneHeartOpening.Text
        | OneSpade oneSpadeOpening -> oneSpadeOpening.Text
        | OneNoTrump oneNoTrumpOpening -> oneNoTrumpOpening.Text

exception UnexpectedHcpForStrengthException of hcp:int<hcp>
exception UnexpectedHcpForBalancedOpeningException of hcp:int<hcp>
exception UnexpectedLengthForOpenedMajorException of length:int
exception UnableToClassifyHandForOneLevelOpeningException of hand:Hand

let private classify (hand:Hand) =
    let strength = function
        | hcp when hcp >= 11<hcp> && hcp <= 15<hcp> -> ElevenToFifteen
        | hcp when hcp >= 16<hcp> && hcp <= 19<hcp> -> SixteenToNineteen
        | hcp -> raise (UnexpectedHcpForStrengthException hcp)
    let spades, hearts, diamonds, clubs = hand.SuitCounts
    match hand.ShapeCategory, hand.Hcp with
    | Balanced, hcp when hcp >= 10<hcp> && hcp <= 21<hcp> ->
        let balancedOpening = function
            | hcp when hcp >= 10<hcp> && hcp <= 12<hcp> -> OneDiamond BalancedTenToTwelve
            | hcp when hcp >= 13<hcp> && hcp <= 15<hcp> -> OneNoTrump BalancedThirteenToFifteen
            | hcp when hcp >= 16<hcp> && hcp <= 18<hcp> -> OneDiamond BalancedSixteenToEighteen
            | hcp when hcp >= 19<hcp> && hcp <= 21<hcp> -> OneClub BalancedNineteenToTwentyOne
            | hcp -> raise (UnexpectedHcpForBalancedOpeningException hcp)
        if spades = 5 && hcp >= 12<hcp> && hcp <= 17<hcp> then
            let spadesHcp = hand.CardsForSuit(Spade) |> List.sumBy (fun card -> card.Rank.Hcp)
            if (float spadesHcp) / (float hcp) > 0.58 then Some (OneSpade (FiveSpadesThreeThreeTwo (strength hcp))) else Some (balancedOpening hcp)
        else if hearts = 5 && hcp >= 12<hcp> && hcp <= 17<hcp> then
            let heartsHcp = hand.CardsForSuit(Heart) |> List.sumBy (fun card -> card.Rank.Hcp)
            if (float heartsHcp) / (float hcp) > 0.58 then Some (OneHeart (FiveHeartsThreeThreeTwo (strength hcp))) else Some (balancedOpening hcp)
        else Some (balancedOpening hcp)
    | _, hcp when hcp >= 11<hcp> && hcp <= 19<hcp> ->
        let openedMajorLength = function
            | length when length = 4 -> ExactlyFourCards
            | length when length > 4 -> FiveOrMoreCards
            | length -> raise (UnexpectedLengthForOpenedMajorException length)
        if hearts >= 4 then
            if hearts >= 6 then Some (OneHeart (LongHearts (strength hcp)))
            else if spades >= 4 && diamonds >= 4 then Some (OneHeart (HeartsSpadesAndDiamonds (strength hcp, openedMajorLength hearts)))
            else if spades >= 4 && clubs >= 4 then Some (OneHeart (HeartsSpadesAndClubs (strength hcp, openedMajorLength hearts)))
            else if diamonds >= 4 && clubs >= 4 then Some (OneHeart (HeartsDiamondsAndClubs (strength hcp, openedMajorLength hearts)))
            else if spades >= 4 then Some (OneHeart (HeartsAndSpades (strength hcp, openedMajorLength hearts)))
            else if diamonds >= 4 then Some (OneHeart (HeartsAndDiamonds (strength hcp, openedMajorLength hearts)))
            else if clubs >= 4 then Some (OneHeart (HeartsAndClubs (strength hcp, openedMajorLength hearts)))
            else raise (UnableToClassifyHandForOneLevelOpeningException hand)
        else if spades >= 4 then
            if spades >= 6 then Some (OneSpade (LongSpades (strength hcp)))
            else if diamonds >= 4 && clubs >= 4 then Some (OneSpade (SpadesDiamondsAndClubs (strength hcp, openedMajorLength spades)))
            else if diamonds >= 4 then Some (OneSpade (SpadesAndDiamonds (strength hcp, openedMajorLength spades)))
            else if clubs >= 4 then Some (OneSpade (SpadesAndClubs (strength hcp, openedMajorLength spades)))
            else raise (UnableToClassifyHandForOneLevelOpeningException hand)
        else if (clubs >= 4 && diamonds >= 5) || (clubs >= 5 && diamonds >= 4) then Some (OneClub (ClubsAndDiamonds (strength hcp)))
        else if (clubs >= 6) then Some (OneClub (LongClubs (strength hcp)))
        else if (diamonds >= 6) then Some (OneClub (LongDiamonds (strength hcp)))
        else raise (UnableToClassifyHandForOneLevelOpeningException hand)
    | _ -> None

let run count =
    if count = 0 then raise CountMustBeGreaterThanZeroException
    let countText = if count = 1 then "hand" else "hands"
    writeNewLine $"Classifying {count} {countText}:\n" ConsoleColor.Magenta
    let generator = Seq.initInfinite (fun _ -> Deal.MakeRandom().Hand(North))
    let mutable classifications = []
    let start = DateTime.UtcNow
    generator |> Seq.take count |> Seq.iter (fun hand ->
        match classify hand with
        | Some classification ->
            (* writeNewLine $"{hand.Text} -> {classification}" ConsoleColor.Red *)
            classifications <- classification :: classifications
        | None ->
            (* writeNewLine $"{hand.Text}" ConsoleColor.DarkRed *)
            ())
    writeBlankLine ()
    let percentage i total =
        let percentage, percent = ((float i) / (float total)) * 100., '%'
        $"{percentage:N2} {percent}"
    let classifiedCount = classifications.Length
    writeNewLine $"{percentage classifiedCount count} of all hands can be opened at the one-level (as dealer)" ConsoleColor.Cyan
    let oneClubOpeners = classifications |> List.choose (fun classification -> match classification with | OneClub _ -> Some classification | _ -> None)
    let oneClub, oneClubOpenersCount = Bid (OneLevel, Suit Club), oneClubOpeners.Length
    writeNewLine $"\n\t-> {percentage oneClubOpenersCount classifiedCount} of hands opened at the one-level ({percentage oneClubOpenersCount count} of all hands) are opened {oneClub.ShortText}\n" ConsoleColor.DarkCyan
    oneClubOpeners |> List.groupBy id |> List.sortBy fst |> List.iter (fun (classification, list) ->
        writeNewLine $"\t\t-> {percentage list.Length oneClubOpenersCount} of hands opened {oneClub.ShortText} ({percentage list.Length count} of all hands) are {classification.Text}" ConsoleColor.DarkGray)
    let oneDiamondOpeners = classifications |> List.choose (fun classification -> match classification with | OneDiamond _ -> Some classification | _ -> None)
    let oneDiamond, oneDiamondOpenersCount = Bid (OneLevel, Suit Diamond), oneDiamondOpeners.Length
    writeNewLine $"\n\t-> {percentage oneDiamondOpenersCount classifiedCount} of hands opened at the one-level ({percentage oneDiamondOpenersCount count} of all hands) are opened {oneDiamond.ShortText}\n" ConsoleColor.DarkCyan
    oneDiamondOpeners |> List.groupBy id |> List.sortBy fst |> List.iter (fun (classification, list) ->
        writeNewLine $"\t\t-> {percentage list.Length oneDiamondOpenersCount} of hands opened {oneDiamond.ShortText} ({percentage list.Length count} of all hands) are {classification.Text}" ConsoleColor.DarkGray)
    let oneHeartOpeners = classifications |> List.choose (fun classification -> match classification with | OneHeart _ -> Some classification | _ -> None)
    let oneHeart, oneHeartOpenersCount = Bid (OneLevel, Suit Heart), oneHeartOpeners.Length
    writeNewLine $"\n\t-> {percentage oneHeartOpenersCount classifiedCount} of hands opened at the one-level ({percentage oneHeartOpenersCount count} of all hands) are opened {oneHeart.ShortText}\n" ConsoleColor.DarkCyan
    oneHeartOpeners |> List.groupBy id |> List.sortBy fst |> List.iter (fun (classification, list) ->
        writeNewLine $"\t\t-> {percentage list.Length oneHeartOpenersCount} of hands opened {oneHeart.ShortText} ({percentage list.Length count} of all hands) are {classification.Text}" ConsoleColor.DarkGray)
    let oneSpadeOpeners = classifications |> List.choose (fun classification -> match classification with | OneSpade _ -> Some classification | _ -> None)
    let oneSpade, oneSpadeOpenersCount = Bid (OneLevel, Suit Spade), oneSpadeOpeners.Length
    writeNewLine $"\n\t-> {percentage oneSpadeOpenersCount classifiedCount} of hands opened at the one-level ({percentage oneSpadeOpenersCount count} of all hands) are opened {oneSpade.ShortText}\n" ConsoleColor.DarkCyan
    oneSpadeOpeners |> List.groupBy id |> List.sortBy fst |> List.iter (fun (classification, list) ->
        writeNewLine $"\t\t-> {percentage list.Length oneSpadeOpenersCount} of hands opened {oneSpade.ShortText} ({percentage list.Length count} of all hands) are {classification.Text}" ConsoleColor.DarkGray)
    let oneNoTrumpOpeners = classifications |> List.choose (fun classification -> match classification with | OneNoTrump _ -> Some classification | _ -> None)
    let oneNoTrump, oneNoTrumpOpenersCount = Bid (OneLevel, NoTrump), oneNoTrumpOpeners.Length
    writeNewLine $"\n\t-> {percentage oneNoTrumpOpenersCount classifiedCount} of hands opened at the one-level ({percentage oneNoTrumpOpenersCount count} of all hands) are opened {oneNoTrump.ShortText}\n" ConsoleColor.DarkCyan
    oneNoTrumpOpeners |> List.groupBy id |> List.sortBy fst |> List.iter (fun (classification, list) ->
        writeNewLine $"\t\t-> {percentage list.Length oneNoTrumpOpenersCount} of hands opened {oneNoTrump.ShortText} ({percentage list.Length count} of all hands) are {classification.Text}" ConsoleColor.DarkGray)
    writeBlankLine ()
    let classifiedCountText = if classifiedCount = 1 then "hand" else "hands"
    writeNewLine $"Classified {classifiedCount} one-level opening {classifiedCountText} (from {count} random {countText}) in {(DateTime.UtcNow - start).TotalSeconds} seconds" ConsoleColor.DarkYellow
