module Aornota.BridgeSim.Domain.Auction

open Aornota.BridgeSim.Domain.Core

type BidSuit = | Suit of Suit | NoTrump
    with
    member this.Rank = match this with | Suit suit -> suit.Rank | NoTrump -> Spade.Rank + 1
    member this.TextSingular = match this with | Suit suit -> suit.Text | NoTrump -> "No Trump"
    member this.TextPlural = $"{this.TextSingular}s"
    member this.ShortText = match this with | Suit suit -> suit.ShortTextUpper | NoTrump -> "NT"

type BidLevel = | OneLevel | TwoLevel | ThreeLevel | FourLevel | FiveLevel | SixLevel | SevenLevel
    with
    member this.Rank = match this with | OneLevel -> 1 | TwoLevel -> 2 | ThreeLevel -> 3 | FourLevel -> 4 | FiveLevel -> 5 | SixLevel -> 6 | SevenLevel -> 7
    member this.Text = match this with | OneLevel -> "One" | TwoLevel -> "Two" | ThreeLevel -> "Three" | FourLevel -> "Four" | FiveLevel -> "Five" | SixLevel -> "Six" | SevenLevel -> "Seven"
    member this.ShortText = $"{this.Rank}"

type Bid = | Pass | Bid of BidLevel * BidSuit | Double | Redouble
    with
    member this.Text = match this with | Pass -> "Pass" | Bid (level, suit) -> $"{level.Text} {if level = OneLevel then suit.TextSingular else suit.TextPlural}" | Double -> "Double" | Redouble -> "Redouble"
    member this.ShortText = match this with | Pass -> "-" | Bid (level, suit) -> $"{level.ShortText}{suit.ShortText}" | Double -> "dbl" | Redouble -> "rdbl"

type Stakes = Undoubled | Doubled | Redoubled
    with
    member this.Text = match this with | Undoubled -> "" | Doubled -> " (doubled)" | Redoubled -> " (redoubled)"
    member this.ShortText = match this with | Undoubled -> "" | Doubled -> "x" | Redoubled -> "xx"

type Contract = | Contract of BidLevel * BidSuit * Stakes * declarer:Position | PassedOut
    with
    member this.ShortText = match this with | Contract (level, suit, stakes, declarer) -> $"{level.ShortText}{suit.ShortText}{stakes.ShortText} by {declarer.Text}" | PassedOut -> "Passed out"

type AuctionState = | Completed of Contract | AwaitingBid of Position * (BidLevel * BidSuit * bool * bool) option

type Auction = private {
    Dealer' : Position
    Bids' : (Position * Bid) list (* head is latest bid *) }
    with
    static member Make(dealer) = { Dealer' = dealer; Bids' = [] }
    member private this.LatestBid =
        match this.Bids' |> List.choose (fun (bidder, bid) -> match bid with | Bid (level, suit) -> Some (bidder, level, suit) | _ -> None) with
        | [] -> None
        | (bidder, position, bid) :: _ -> Some (bidder, position, bid)
    member private this.LatestNonPass = match this.Bids' |> List.filter (fun (_, bid) -> bid <> Pass) with | [] -> None | (position, bid) :: _ -> Some (position, bid)
    member private this.DoubledBy = match this.LatestNonPass with | Some (position, bid) when bid = Double -> Some position | _ -> None
    member private this.RedoubledBy = match this.LatestNonPass with | Some (position, bid) when bid = Redouble -> Some position | _ -> None
    member private this.CurrentPassCount =
        let rec count auction acc = match auction with | [] -> acc | (_, bid) :: t -> if bid = Pass then count t (acc + 1) else acc
        count this.Bids' 0
    member private this.Contract =
        match this.LatestBid, this.CurrentPassCount with
        | None, 4 -> Some PassedOut
        | Some (bidder, level, suit), 3 ->
            let declarer =
                match this.Bids' |> List.rev |> List.filter (fun (position, bid') -> match bid' with | Bid (_, suit') -> suit' = suit && not (position.IsOpponent(bidder)) | _ -> false) with
                | [] -> failwith $"SHOULD NEVER HAPPEN -> Unable to ascetion who first bid {suit.TextPlural} (out of {bidder.Text} and {bidder.Partner.Text})"
                | (position, _) :: _ -> position
            let stakes =
                match this.DoubledBy.IsSome, this.RedoubledBy.IsSome with
                | false, false -> Undoubled
                | true, false -> Doubled
                | false, true -> Redoubled
                | true, true -> failwith "SHOULD NEVER HAPPEN -> Latest (non-pass) bid cannot be both double and redouble"
            Some (Contract (level, suit, stakes, declarer))
        | _ -> None
    member this.State =
        match this.Contract with
        | Some contract -> Completed contract
        | None ->
            let nextBidder = match this.Bids' with | [] -> this.Dealer' | (position, _) :: _ -> position.LHO
            match this.LatestBid with
            | None -> AwaitingBid (nextBidder, None)
            | Some (bidder, level, suit) ->
                let canDouble = this.DoubledBy.IsNone && bidder.IsOpponent(nextBidder)
                let canRedouble = this.DoubledBy.IsSome && this.RedoubledBy.IsNone && bidder.IsPartnership(nextBidder)
                AwaitingBid (nextBidder, Some (level, suit, canDouble, canRedouble))
    member this.Bid(bidder:Position, bid) =
        match this.State with
        | Completed contract ->failwith $"{bidder.Text} cannot bid because the auction is complete (contract is {contract})"
        | AwaitingBid (nextBidder, latestBid) ->
            if nextBidder <> bidder then failwith $"""{bidder.Text} cannot bid because the {match this.Bids' with | [] -> "first" | _ -> "next"} bidder should be {nextBidder.Text}"""
            match latestBid with
            | None ->
                match bid with
                | Double | Redouble -> failwith $"{bidder.Text} cannot double or redouble"
                | _ -> { this with Bids' = (bidder, bid) :: this.Bids' }
            | Some (level, suit, canDouble, canRedouble) ->
                match bid with
                | Pass -> { this with Bids' = (bidder, bid) :: this.Bids' }
                | Bid (level', suit') ->
                    if level'.Rank < level.Rank || (level'.Rank = level.Rank && suit'.Rank <= suit.Rank) then failwith $"{bidder.Text}'s bid must be higher than {level.ShortText}{suit.ShortText}"
                    { this with Bids' = (bidder, bid) :: this.Bids' }
                | Double ->
                    if not canDouble then failwith $"{bidder.Text} cannot double"
                    { this with Bids' = (bidder, bid) :: this.Bids' }
                | Redouble ->
                    if not canRedouble then failwith $"{bidder.Text} cannot redouble"
                    { this with Bids' = (bidder, bid) :: this.Bids' }
    member this.Dealer = this.Dealer'
    member this.Bids = this.Bids' |> List.rev
