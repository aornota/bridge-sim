module Aornota.BridgeSim.Domain.Simulation.ComparisonConstraint

open Aornota.BridgeSim.Domain.Core

// TODO-NMB: Make ComparisonConstraint<'a> private - and add HandHcpConstraint (&c.)...

type ComparisonConstraint<'a> when 'a:comparison =
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

exception MinimumHcpMustBeBetweenException of int<hcp> * int<hcp> * int<hcp>
exception MaximumHcpMustBeBetweenException of int<hcp> * int<hcp> * int<hcp>
exception MinimumHcpMustBeLessThanMaximumHcpException of int<hcp> * int<hcp>
exception ExactHcpMustBeBetweenException of int<hcp> * int<hcp> * int<hcp>

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
        if min < HandHcpConstraint.MinMin || min > HandHcpConstraint.MaxMin then raise (MinimumHcpMustBeBetweenException (min, HandHcpConstraint.MinMin, HandHcpConstraint.MaxMin))
        { HcpConstraint' = AtLeast min }
    static member AtMost max =
        let max = max * 1<hcp>
        if max < HandHcpConstraint.MinMax || max > HandHcpConstraint.MaxMax then raise (MaximumHcpMustBeBetweenException (max, HandHcpConstraint.MinMax, HandHcpConstraint.MaxMax))
        { HcpConstraint' = AtMost max }
    static member Between (min, max) =
        let min, max = min * 1<hcp>, max * 1<hcp>
        if max <= min then raise (MinimumHcpMustBeLessThanMaximumHcpException (min, max))
        if min < HandHcpConstraint.ImplicitMin || min > HandHcpConstraint.MaxMax then raise (MinimumHcpMustBeBetweenException (min, HandHcpConstraint.ImplicitMin, HandHcpConstraint.MaxMax))
        if max < HandHcpConstraint.MinMin || max > HandHcpConstraint.ImplicitMax then raise (MaximumHcpMustBeBetweenException (max, HandHcpConstraint.MinMin, HandHcpConstraint.ImplicitMax))
        { HcpConstraint' = Between (min, max) }
    static member Exactly exact =
        let exact = exact * 1<hcp>
        if exact < HandHcpConstraint.ImplicitMin || exact > HandHcpConstraint.ImplicitMax then raise (ExactHcpMustBeBetweenException (exact, HandHcpConstraint.ImplicitMin, HandHcpConstraint.ImplicitMax))
        { HcpConstraint' = Exactly exact }
    member this.Min = this.HcpConstraint'.Min |> Option.defaultValue HandHcpConstraint.ImplicitMin
    member this.Max = this.HcpConstraint'.Max |> Option.defaultValue HandHcpConstraint.ImplicitMax
    member this.Matches value = this.HcpConstraint'.Matches value
    member this.Text = $"{this.HcpConstraint'.Text} HCP"

// TODO-NMB...type CcConstraint =

// TODO-NMB...type SuitLengthConstraint =
