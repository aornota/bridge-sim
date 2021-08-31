module Aornota.BridgeSim.Domain.Extensions.Deal

open Aornota.BridgeSim.Domain.Auction
open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Extensions.Auction
open Aornota.BridgeSim.Domain.Extensions.Core

type Deal
    with
    member this.Diagram =
        // TODO-NMB: Implement diagram...

        [ "" ] // TEMP-NMB
    member this.AuctionDiagram = auctionDiagram (this.Dealer, this.AuctionState, this.Bids)
