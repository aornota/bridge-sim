module Aornota.BridgeSim.Domain.Simulation.ComparisonConstraint

open Aornota.BridgeSim.Domain.Core

type private ComparisonConstraint<'a> when 'a:comparison =
    | AtLeast of 'a
    | AtMost of 'a
    | Between of 'a * 'a
    | Exactly of 'a
    with
    member this.Min =
        match this with
        | AtLeast min -> Some min
        | AtMost _ -> None
        | Between (min, _) -> Some min
        | Exactly exact -> Some exact
    member this.Max =
        match this with
        | AtLeast _ -> None
        | AtMost max -> Some max
        | Between (_, max) -> Some max
        | Exactly exact -> Some exact
    member this.Matches (value) =
        match this with
        | AtLeast min -> value >= min
        | AtMost max -> value <= max
        | Between (min, max) -> value >= min && value <= max
        | Exactly exact -> value = exact
    member this.Text =
        match this with
        | AtLeast min -> $">= {min}"
        | AtMost max -> $"<= {max}"
        | Between (min, max) -> $"{min}-{max}"
        | Exactly exact -> $"exactly {exact}"

exception MinimumHandHcpMustBeBetweenException of int<hcp> * int<hcp> * int<hcp>
exception MaximumHandHcpMustBeBetweenException of int<hcp> * int<hcp> * int<hcp>
exception MinimumHandHcpMustBeLessThanMaximumHandHcpException of int<hcp> * int<hcp>
exception ExactHandHcpMustBeBetweenException of int<hcp> * int<hcp> * int<hcp>

type HandHcpConstraint =
    private { HcpConstraint': ComparisonConstraint<int<hcp>>}
    with
    static member private ImplicitMin = 0<hcp>
    static member private ImplicitMax = 37<hcp>
    static member private MinMin = HandHcpConstraint.ImplicitMin + 1<hcp>
    static member private MaxMin = HandHcpConstraint.ImplicitMax
    static member private MinMax = HandHcpConstraint.ImplicitMin
    static member private MaxMax = HandHcpConstraint.ImplicitMax - 1<hcp>
    static member AtLeast min =
        let min = min * 1<hcp>
        if min < HandHcpConstraint.MinMin || min > HandHcpConstraint.MaxMin then raise (MinimumHandHcpMustBeBetweenException (min, HandHcpConstraint.MinMin, HandHcpConstraint.MaxMin))
        { HcpConstraint' = AtLeast min }
    static member AtMost max =
        let max = max * 1<hcp>
        if max < HandHcpConstraint.MinMax || max > HandHcpConstraint.MaxMax then raise (MaximumHandHcpMustBeBetweenException (max, HandHcpConstraint.MinMax, HandHcpConstraint.MaxMax))
        { HcpConstraint' = AtMost max }
    static member Between (min, max) =
        let min, max = min * 1<hcp>, max * 1<hcp>
        if max <= min then raise (MinimumHandHcpMustBeLessThanMaximumHandHcpException (min, max))
        if min < HandHcpConstraint.ImplicitMin || min > HandHcpConstraint.MaxMax then raise (MinimumHandHcpMustBeBetweenException (min, HandHcpConstraint.ImplicitMin, HandHcpConstraint.MaxMax))
        if max < HandHcpConstraint.MinMin || max > HandHcpConstraint.ImplicitMax then raise (MaximumHandHcpMustBeBetweenException (max, HandHcpConstraint.MinMin, HandHcpConstraint.ImplicitMax))
        { HcpConstraint' = Between (min, max) }
    static member Exactly exact =
        let exact = exact * 1<hcp>
        if exact < HandHcpConstraint.ImplicitMin || exact > HandHcpConstraint.ImplicitMax then raise (ExactHandHcpMustBeBetweenException (exact, HandHcpConstraint.ImplicitMin, HandHcpConstraint.ImplicitMax))
        { HcpConstraint' = Exactly exact }
    member this.Min = this.HcpConstraint'.Min |> Option.defaultValue HandHcpConstraint.ImplicitMin
    member this.Max = this.HcpConstraint'.Max |> Option.defaultValue HandHcpConstraint.ImplicitMax
    member this.Matches value = this.HcpConstraint'.Matches value
    member this.Text = $"{this.HcpConstraint'.Text} HCP"

exception MinimumPartnershipHcpMustBeBetweenException of int<hcp> * int<hcp> * int<hcp>
exception MaximumPartnershipHcpMustBeBetweenException of int<hcp> * int<hcp> * int<hcp>
exception MinimumPartnershipHcpMustBeLessThanMaximumPartnershipHcpException of int<hcp> * int<hcp>
exception ExactPartnershipHcpMustBeBetweenException of int<hcp> * int<hcp> * int<hcp>

type PartnershipHcpConstraint =
    private { HcpConstraint': ComparisonConstraint<int<hcp>>}
    with
    static member private ImplicitMin = 0<hcp>
    static member private ImplicitMax = 40<hcp>
    static member private MinMin = PartnershipHcpConstraint.ImplicitMin + 1<hcp>
    static member private MaxMin = PartnershipHcpConstraint.ImplicitMax
    static member private MinMax = PartnershipHcpConstraint.ImplicitMin
    static member private MaxMax = PartnershipHcpConstraint.ImplicitMax - 1<hcp>
    static member AtLeast min =
        let min = min * 1<hcp>
        if min < PartnershipHcpConstraint.MinMin || min > PartnershipHcpConstraint.MaxMin then raise (MinimumPartnershipHcpMustBeBetweenException (min, PartnershipHcpConstraint.MinMin, PartnershipHcpConstraint.MaxMin))
        { HcpConstraint' = AtLeast min }
    static member AtMost max =
        let max = max * 1<hcp>
        if max < PartnershipHcpConstraint.MinMax || max > PartnershipHcpConstraint.MaxMax then raise (MaximumPartnershipHcpMustBeBetweenException (max, PartnershipHcpConstraint.MinMax, PartnershipHcpConstraint.MaxMax))
        { HcpConstraint' = AtMost max }
    static member Between (min, max) =
        let min, max = min * 1<hcp>, max * 1<hcp>
        if max <= min then raise (MinimumPartnershipHcpMustBeLessThanMaximumPartnershipHcpException (min, max))
        if min < PartnershipHcpConstraint.ImplicitMin || min > PartnershipHcpConstraint.MaxMax then raise (MinimumPartnershipHcpMustBeBetweenException (min, PartnershipHcpConstraint.ImplicitMin, PartnershipHcpConstraint.MaxMax))
        if max < PartnershipHcpConstraint.MinMin || max > PartnershipHcpConstraint.ImplicitMax then raise (MaximumPartnershipHcpMustBeBetweenException (max, PartnershipHcpConstraint.MinMin, PartnershipHcpConstraint.ImplicitMax))
        { HcpConstraint' = Between (min, max) }
    static member Exactly exact =
        let exact = exact * 1<hcp>
        if exact < PartnershipHcpConstraint.ImplicitMin || exact > PartnershipHcpConstraint.ImplicitMax then raise (ExactPartnershipHcpMustBeBetweenException (exact, PartnershipHcpConstraint.ImplicitMin, PartnershipHcpConstraint.ImplicitMax))
        { HcpConstraint' = Exactly exact }
    member this.Min = this.HcpConstraint'.Min |> Option.defaultValue PartnershipHcpConstraint.ImplicitMin
    member this.Max = this.HcpConstraint'.Max |> Option.defaultValue PartnershipHcpConstraint.ImplicitMax
    member this.Matches value = this.HcpConstraint'.Matches value
    member this.Text = $"{this.HcpConstraint'.Text} HCP"

exception MinimumCcMustBeBetweenException of int<cc> * int<cc> * int<cc>
exception MaximumCcMustBeBetweenException of int<cc> * int<cc> * int<cc>
exception MinimumCcMustBeLessThanMaximumCcException of int<cc> * int<cc>
exception ExactCcMustBeBetweenException of int<cc> * int<cc> * int<cc>

type CcConstraint =
    private { CcConstraint': ComparisonConstraint<int<cc>>}
    with
    static member private ImplicitMin = 0<cc>
    static member private ImplicitMax = 12<cc>
    static member private MinMin = CcConstraint.ImplicitMin + 1<cc>
    static member private MaxMin = CcConstraint.ImplicitMax
    static member private MinMax = CcConstraint.ImplicitMin
    static member private MaxMax = CcConstraint.ImplicitMax - 1<cc>
    static member AtLeast min =
        let min = min * 1<cc>
        if min < CcConstraint.MinMin || min > CcConstraint.MaxMin then raise (MinimumCcMustBeBetweenException (min, CcConstraint.MinMin, CcConstraint.MaxMin))
        { CcConstraint' = AtLeast min }
    static member AtMost max =
        let max = max * 1<cc>
        if max < CcConstraint.MinMax || max > CcConstraint.MaxMax then raise (MaximumCcMustBeBetweenException (max, CcConstraint.MinMax, CcConstraint.MaxMax))
        { CcConstraint' = AtMost max }
    static member Between (min, max) =
        let min, max = min * 1<cc>, max * 1<cc>
        if max <= min then raise (MinimumCcMustBeLessThanMaximumCcException (min, max))
        if min < CcConstraint.ImplicitMin || min > CcConstraint.MaxMax then raise (MinimumCcMustBeBetweenException (min, CcConstraint.ImplicitMin, CcConstraint.MaxMax))
        if max < CcConstraint.MinMin || max > CcConstraint.ImplicitMax then raise (MaximumCcMustBeBetweenException (max, CcConstraint.MinMin, CcConstraint.ImplicitMax))
        { CcConstraint' = Between (min, max) }
    static member Exactly exact =
        let exact = exact * 1<cc>
        if exact < CcConstraint.ImplicitMin || exact > CcConstraint.ImplicitMax then raise (ExactCcMustBeBetweenException (exact, CcConstraint.ImplicitMin, CcConstraint.ImplicitMax))
        { CcConstraint' = Exactly exact }
    member this.Min = this.CcConstraint'.Min |> Option.defaultValue CcConstraint.ImplicitMin
    member this.Max = this.CcConstraint'.Max |> Option.defaultValue CcConstraint.ImplicitMax
    member this.Matches value = this.CcConstraint'.Matches value
    member this.Text = $"{this.CcConstraint'.Text} CC"

exception MinimumSuitLengthMustBeBetweenException of int * int * int
exception MaximumSuitLengthMustBeBetweenException of int * int * int
exception MinimumSuitLengthMustBeLessThanMaximumSuitLengthException of int * int
exception ExactSuitLengthMustBeBetweenException of int * int * int

type SuitConstraint =
    private { SuitConstraint': ComparisonConstraint<int>}
    with
    static member ImplicitMin = 0
    static member ImplicitMax = CARDS_PER_HAND
    static member private MinMin = SuitConstraint.ImplicitMin + 1
    static member private MaxMin = SuitConstraint.ImplicitMax
    static member private MinMax = SuitConstraint.ImplicitMin
    static member private MaxMax = SuitConstraint.ImplicitMax - 1
    static member AtLeast min =
        if min < SuitConstraint.MinMin || min > SuitConstraint.MaxMin then raise (MinimumSuitLengthMustBeBetweenException (min, SuitConstraint.MinMin, SuitConstraint.MaxMin))
        { SuitConstraint' = AtLeast min }
    static member AtMost max =
        if max < SuitConstraint.MinMax || max > SuitConstraint.MaxMax then raise (MaximumSuitLengthMustBeBetweenException (max, SuitConstraint.MinMax, SuitConstraint.MaxMax))
        { SuitConstraint' = AtMost max }
    static member Between (min, max) =
        if max <= min then raise (MinimumSuitLengthMustBeLessThanMaximumSuitLengthException (min, max))
        if min < SuitConstraint.ImplicitMin || min > SuitConstraint.MaxMax then raise (MinimumSuitLengthMustBeBetweenException (min, SuitConstraint.ImplicitMin, SuitConstraint.MaxMax))
        if max < SuitConstraint.MinMin || max > SuitConstraint.ImplicitMax then raise (MaximumSuitLengthMustBeBetweenException (max, SuitConstraint.MinMin, SuitConstraint.ImplicitMax))
        { SuitConstraint' = Between (min, max) }
    static member Exactly exact =
        if exact < SuitConstraint.ImplicitMin || exact > SuitConstraint.ImplicitMax then raise (ExactSuitLengthMustBeBetweenException (exact, SuitConstraint.ImplicitMin, SuitConstraint.ImplicitMax))
        { SuitConstraint' = Exactly exact }
    member this.Min = this.SuitConstraint'.Min
    member this.Max = this.SuitConstraint'.Max
    member this.Matches value = this.SuitConstraint'.Matches value
    member this.Text = $"{this.SuitConstraint'.Text}"
