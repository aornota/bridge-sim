module Aornota.BridgeSim.Domain.Extensions.Core

open Aornota.BridgeSim.Domain.Core

type Hand
    with
    member this.SpecificShapeText =
        let spadeCount, heartCount, diamondCount, clubCount = this.SuitCounts
        $"{spadeCount}={heartCount}={diamondCount}={clubCount}"
    member this.Text = $"{this.CardsText(Spade)} {this.CardsText(Heart)} {this.CardsText(Diamond)} {this.CardsText(Club)} -- {this.Hcp} HCP | {this.ShapeCategory.TextLower} ({this.SpecificShapeText})"
