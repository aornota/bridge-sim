module Aornota.BridgeSim.Domain.Simulation.Constraint

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
