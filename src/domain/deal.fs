module Aornota.BridgeSim.Domain.Deal

open Aornota.BridgeSim.Domain.Auction
open Aornota.BridgeSim.Domain.Core

type Partnership = NorthSouth | EastWest
    with
    static member ForPosition(position) = match position with North | South -> NorthSouth | East | West -> EastWest
    member this.Opposition = match this with | NorthSouth -> EastWest | EastWest -> NorthSouth
    member this.Text = match this with | NorthSouth -> $"{North.Text}/{South.Text}" | EastWest -> $"{East.Text}/{West.Text}"
    member this.ShortText = match this with | NorthSouth -> $"{North.ShortText}/{South.ShortText}" | EastWest -> $"{East.ShortText}/{West.ShortText}"

type RelativeVulnerability = | Favourable | Equal | Unfavourable
    with
    member this.Text = match this with | Favourable -> "Favourable" | Equal -> "Equal" | Unfavourable -> "Unfavourable"

type Vulnerabilities = private {
    NorthSouthVulnerability' : Vulnerabilty
    EastWestVulnerability' : Vulnerabilty }
    with
    static member Make(northSouthVulnerability, eastWestVulnerability) = { NorthSouthVulnerability' = northSouthVulnerability ; EastWestVulnerability' = eastWestVulnerability }
    member this.Vulnerability(partnership) = match partnership with | NorthSouth -> this.NorthSouthVulnerability' | EastWest -> this.EastWestVulnerability'
    member this.Vulnerability(position) = this.Vulnerability(Partnership.ForPosition(position))
    member this.RelativeVulnerability(partnership:Partnership) =
        match this.Vulnerability(partnership), this.Vulnerability(partnership.Opposition) with
        | NotVulnerable, Vulnerable -> Favourable
        | NotVulnerable, NotVulnerable | Vulnerable, Vulnerable -> Equal
        | Vulnerable, NotVulnerable -> Unfavourable
    member this.Text = $"{NorthSouth.ShortText} {this.NorthSouthVulnerability'.TextLower} | {EastWest.ShortText} {this.EastWestVulnerability'.TextLower}"
    member this.ShortText = $"{NorthSouth.ShortText} {this.NorthSouthVulnerability'.ShortTextLower} | {EastWest.ShortText} {this.EastWestVulnerability'.ShortTextLower}"
    member this.SummaryText =
        match this.NorthSouthVulnerability', this.EastWestVulnerability' with
        | NotVulnerable, NotVulnerable -> $"Neither {Vulnerable.ShortTextLower}"
        | NotVulnerable, Vulnerable -> $"{EastWest.ShortText} {Vulnerable.ShortTextLower}"
        | Vulnerable, NotVulnerable -> $"{NorthSouth.ShortText} {Vulnerable.ShortTextLower}"
        | Vulnerable, Vulnerable -> $"Both {Vulnerable.ShortTextLower}"

type Seat = | FirstSeat | SecondSeat | ThirdSeat | FourthSeat
    with
    member this.Text = match this with | FirstSeat -> "First seat" | SecondSeat -> "Second seat" | ThirdSeat -> "Third seat" | FourthSeat -> "Fourth seat"
    member this.ShortText = match this with | FirstSeat -> "1st seat" | SecondSeat -> "2nd seat" | ThirdSeat -> "3rd seat" | FourthSeat -> "4th seat"

type Deal = private {
    Vulnerabilities' : Vulnerabilities
    FirstSeatHand' : Hand
    SecondSeatHand' : Hand
    ThirdSeatHand' : Hand
    FourthSeatHand' : Hand
    Auction' : Auction } // TODO-NMB: More?...
    with
    static member Make(dealer, northSouthVulnerability, eastWestVulnerability) =
        let deck = Deck.MakeShuffled()
        let firstSeatCards, deck = deck.Deal(CARDS_PER_HAND)
        let secondSeatCards, deck = deck.Deal(CARDS_PER_HAND)
        let thirdSeatCards, deck = deck.Deal(CARDS_PER_HAND)
        let fourthSeatCards, _ = deck.Deal(CARDS_PER_HAND)
        {
            Vulnerabilities' = Vulnerabilities.Make(northSouthVulnerability, eastWestVulnerability)
            FirstSeatHand' = Hand.Make(firstSeatCards)
            SecondSeatHand' = Hand.Make(secondSeatCards)
            ThirdSeatHand' = Hand.Make(thirdSeatCards)
            FourthSeatHand' = Hand.Make(fourthSeatCards)
            Auction' = Auction.Make(dealer)
        }
    member this.Dealer = this.Auction'.Dealer
    member this.Vulnerabilities = this.Vulnerabilities'
    member this.SeatAndHand(position) =
        match this.Auction'.Dealer, position with
        | North, North | East, East | South, South | West, West -> FirstSeat, this.FirstSeatHand'
        | North, East | East, South | South, West | West, North -> SecondSeat, this.SecondSeatHand'
        | North, South | East, West | South, North | West, East -> ThirdSeat, this.ThirdSeatHand'
        | North, West | East, North | South, East | West, South -> FourthSeat, this.FourthSeatHand'
    member this.Hand(position) = snd (this.SeatAndHand(position))
    member this.Seat(position) = fst (this.SeatAndHand(position))
    member this.AuctionState = this.Auction'.State
    member this.Bids = this.Auction'.Bids
    member this.Bid(bidder, bid) = { this with Auction' = this.Auction'.Bid(bidder, bid) }
