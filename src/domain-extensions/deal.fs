module Aornota.BridgeSim.Domain.Extensions.Deal

open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Extensions.Auction
open Aornota.BridgeSim.Domain.Extensions.Core

type Deal
    with
    member private this.PositionText(position) = if position = this.Dealer then $"|{position.ShortText}|" else $" {position.ShortText} "
    member private this.PartnershipSummary(partnership) =
        let hand1, hand2 = match partnership with | NorthSouth -> this.Hand(North), this.Hand(South) | EastWest -> this.Hand(East), this.Hand(West)
        let spades1, hearts1, diamonds1, clubs1 = hand1.SuitCounts
        let spades2, hearts2, diamonds2, clubs2 = hand2.SuitCounts
        let shapeText = $"{spades1 + spades2}={hearts1 + hearts2}={diamonds1 + diamonds2}={clubs1 + clubs2}"
        $"{partnership.ShortText} -> {hand1.Hcp + hand2.Hcp} HCP | {shapeText} | {this.Vulnerabilities.Vulnerability(partnership).TextLower}"
    member this.Summary(withPartnershipSummaries) =
        let northHand, eastHand, southHand, westHand = this.Hand(North), this.Hand(East), this.Hand(South), this.Hand(West)
        [
            $"{this.PositionText(North)} -> {northHand.Text}"
            $"{this.PositionText(East)} -> {eastHand.Text}"
            $"{this.PositionText(South)} -> {southHand.Text}"
            $"{this.PositionText(West)} -> {westHand.Text}"
            if withPartnershipSummaries then
                ""
                this.PartnershipSummary(NorthSouth)
                this.PartnershipSummary(EastWest)
        ]
    member this.Diagram(withPartnershipSummaries) =
        let addSpacesLeft count (text:string) = $"""{String.replicate count " "}{text}"""
        let padRight width (text:string) = if text.Length >= width then text.Substring(0, width) else $"""{text}{String.replicate (width - text.Length) " "}"""
        let shapeText (hand:Hand) = $"{hand.ShapeCategory.TextUpper} ({hand.SpecificShapeText})"
        let hcpText (hand:Hand) = $"{hand.Hcp} HCP"
        let northHand, eastHand, southHand, westHand = this.Hand(North), this.Hand(East), this.Hand(South), this.Hand(West)
        let northShapeText = shapeText northHand
        let northWidth = northShapeText.Length
        let northSpades, northHearts, northDiamonds, northClubs = northHand.CardsText(Spade), northHand.CardsText(Heart), northHand.CardsText(Diamond), northHand.CardsText(Club)
        let southShapeText = shapeText southHand
        let southWidth = southShapeText.Length
        let southSpades, southHearts, southDiamonds, southClubs = southHand.CardsText(Spade), southHand.CardsText(Heart), southHand.CardsText(Diamond), southHand.CardsText(Club)
        let northSouthCardsMax = [ northSpades ; northHearts ; northDiamonds ; northClubs ; southSpades ; southHearts ; southDiamonds ; southClubs ] |> List.map (fun text -> text.Length) |> List.max
        let northSouthIndent = (max northWidth southWidth- northSouthCardsMax) / 2
        let northShapeIndent = if northWidth > southWidth then 0 else (southWidth - northWidth) / 2
        let southShapeIndent = if southWidth > northWidth then 0 else (northWidth - southWidth) / 2
        let eastShapeText = shapeText eastHand
        let eastWidth = eastShapeText.Length
        let eastSpades, eastHearts, eastDiamonds, eastClubs = eastHand.CardsText(Spade), eastHand.CardsText(Heart), eastHand.CardsText(Diamond), eastHand.CardsText(Club)
        let eastCardsMax = [ eastSpades ; eastHearts ; eastDiamonds ; eastClubs ] |> List.map (fun text -> text.Length) |> List.max
        let eastIndent = (eastWidth - eastCardsMax) / 2
        let westShapeText = shapeText westHand
        let westWidth = westShapeText.Length
        let westSpades, westHearts, westDiamonds, westClubs = westHand.CardsText(Spade), westHand.CardsText(Heart), westHand.CardsText(Diamond), westHand.CardsText(Club)
        let westCardsMax = [ westSpades ; westHearts ; westDiamonds ; westClubs ] |> List.map (fun text -> text.Length) |> List.max
        let westIndent = (westWidth - westCardsMax) / 2
        [
            $"{this.Vulnerabilities.SummaryText |> padRight (westWidth + 2 + northSouthIndent)}{northSpades}"
            northHearts |> addSpacesLeft (westWidth + 2 + northSouthIndent)
            northDiamonds |> addSpacesLeft (westWidth + 2 + northSouthIndent)
            northClubs |> addSpacesLeft (westWidth + 2 + northSouthIndent)
            ""
            hcpText northHand |> addSpacesLeft (westWidth + 2 + northSouthIndent)
            northShapeText |> addSpacesLeft (westWidth + 2 + northShapeIndent)
            ""
            $"{westSpades |> addSpacesLeft westIndent |> padRight (westWidth + northSouthIndent + 15)}{eastSpades |> addSpacesLeft eastIndent}"
            $"{westHearts |> addSpacesLeft westIndent |> padRight (westWidth + northSouthIndent+ 3)}{this.PositionText(North) |> padRight 12}{eastHearts |> addSpacesLeft eastIndent}"
            $"{westDiamonds |> addSpacesLeft westIndent |> padRight (westWidth + northSouthIndent + 15)}{eastDiamonds |> addSpacesLeft eastIndent}"
            $"{westClubs |> addSpacesLeft westIndent|> padRight ((westWidth + northSouthIndent) - 2)}{this.PositionText(West) |> padRight 10}{this.PositionText(East) |> padRight 7}{eastClubs |> addSpacesLeft eastIndent}"
            ""
            $"{hcpText westHand |> addSpacesLeft westIndent|> padRight (westWidth + northSouthIndent + 3)}{this.PositionText(South) |> padRight 12}{hcpText eastHand |> addSpacesLeft eastIndent}"
            $"{westShapeText |> padRight (westWidth + northSouthIndent + 15)}{shapeText eastHand}"
            ""
            southSpades |> addSpacesLeft (westWidth + 2 + northSouthIndent)
            southHearts |> addSpacesLeft (westWidth + 2 + northSouthIndent)
            southDiamonds |> addSpacesLeft (westWidth + 2 + northSouthIndent)
            southClubs |> addSpacesLeft (westWidth + 2 + northSouthIndent)
            ""
            hcpText southHand |> addSpacesLeft (westWidth + 2 + northSouthIndent)
            southShapeText |> addSpacesLeft (westWidth + 2 + southShapeIndent)
            if withPartnershipSummaries then
                ""
                this.PartnershipSummary(NorthSouth)
                this.PartnershipSummary(EastWest)
        ]
    member this.AuctionDiagram = auctionDiagram (this.Dealer, this.AuctionState, this.Bids)
