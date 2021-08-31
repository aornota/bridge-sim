module Aornota.BridgeSim.Domain.Extensions.Auction

open Aornota.BridgeSim.Domain.Auction
open Aornota.BridgeSim.Domain.Core

let [<Literal>] private NT_FIRST_TRICK = 40
let [<Literal>] private NT_SUBSEQUENT_TRICK = 30
let [<Literal>] private MAJOR_SUIT_TRICK = 30
let [<Literal>] private MINOR_SUIT_TRICK = 20

let [<Literal>] private CONTRACT_POINTS_FOR_GAME = 100

type Contract
    with
    member this.DuplicateScore(vulnerability:Vulnerabilty, tricksTaken:uint) =
        match this with
        | Contract (level, strain, stakes, _) ->
            let perSuitTrick (suit:Suit) = if suit.IsMajor then MAJOR_SUIT_TRICK else MINOR_SUIT_TRICK
            let contractPoints =
                match max (int (min level.TricksRequired tricksTaken) - 6) 0, strain with
                | 0, _ -> 0
                | n, NoTrump -> NT_FIRST_TRICK + ((n - 1) * NT_SUBSEQUENT_TRICK)
                | n, Suit suit -> n * perSuitTrick suit
            let contractPoints = contractPoints * match stakes with | Undoubled -> 1 | Doubled -> 2 | Redoubled -> 4
            let overtricks = max (int tricksTaken - int level.TricksRequired) 0
            let overtrickPoints =
                match overtricks, vulnerability, stakes, strain with
                | 0, _, _, _ -> 0
                | n, _, Undoubled, NoTrump -> n * NT_SUBSEQUENT_TRICK
                | n, _, Undoubled, Suit suit -> n * perSuitTrick suit
                | n, NotVulnerable, Doubled, _ -> n * 100
                | n, NotVulnerable, Redoubled, _ -> n * 200
                | n, Vulnerable, Doubled, _ -> n * 200
                | n, Vulnerable, Redoubled, _ -> n * 400
            let undertricks = max (int level.TricksRequired - int tricksTaken) 0
            let doubledOrRedoubledBonusPoints =
                match undertricks, stakes with
                | 0, Doubled -> 50
                | 0, Redoubled -> 100
                | _ -> 0
            let slamBonusPoints =
                match undertricks, level, vulnerability with
                | 0, SixLevel, NotVulnerable -> 500
                | 0, SixLevel, Vulnerable -> 750
                | 0, SevenLevel, NotVulnerable -> 1000
                | 0, SevenLevel, Vulnerable -> 1500
                | _ -> 0
            let gameOrPartScoreBonusPoints =
                match undertricks, contractPoints, vulnerability with
                | 0, p, NotVulnerable when p >= CONTRACT_POINTS_FOR_GAME -> 300
                | 0, p, Vulnerable when p >= CONTRACT_POINTS_FOR_GAME -> 500
                | 0, _, _ -> 50
                | _ -> 0
            let penaltyPoints =
                match undertricks, vulnerability, stakes with
                | 0, _, _ -> 0
                | n, NotVulnerable, Undoubled -> n * 50
                | 1, NotVulnerable, Doubled -> 100
                | n, NotVulnerable, Doubled when n <= 3 -> 100 + ((n - 1) * 200)
                | n, NotVulnerable, Doubled -> 100 + (2 * 200) + ((n - 3) * 300)
                | 1, NotVulnerable, Redoubled -> 200
                | n, NotVulnerable, Redoubled when n <= 3 -> 200 + ((n - 1) * 400)
                | n, NotVulnerable, Redoubled -> 200 + (2 * 400) + ((n - 3) * 600)
                | n, Vulnerable, Undoubled -> n * 100
                | 1, Vulnerable, Doubled -> 200
                | n, Vulnerable, Doubled -> 200 + ((n - 1) * 300)
                | 1, Vulnerable, Redoubled -> 400
                | n, Vulnerable, Redoubled -> 400 + ((n - 1) * 600)
            (contractPoints + overtrickPoints + doubledOrRedoubledBonusPoints + slamBonusPoints + gameOrPartScoreBonusPoints) - penaltyPoints
        | PassedOut -> 0

let auctionDiagram (dealer, state, bids) =
    // TODO-NMB: Implement diagram...

    [ "" ] // TEMP-NMB

type Auction
    with
    member this.Diagram = auctionDiagram (this.Dealer, this.State, this.Bids)
