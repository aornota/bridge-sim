module Aornota.BridgeSim.Domain.Extensions.Core

open Aornota.BridgeSim.Domain.Core

type Hand
    with
    member this.SpecificShapeText =
        let spadeCount, heartCount, diamondCount, clubCount = this.SuitCounts
        $"{spadeCount}={heartCount}={diamondCount}={clubCount}"
    member this.Text = $"{this.CardsText} -- {this.ShapeCategory.TextLower} ({this.SpecificShapeText}) | {this.Hcp} HCP"
