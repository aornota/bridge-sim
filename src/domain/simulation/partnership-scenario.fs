module Aornota.BridgeSim.Domain.Simulation.PartnershipScenario

open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Evaluation.Core
open Aornota.BridgeSim.Domain.Formatting.Core
open Aornota.BridgeSim.Domain.Formatting.Deal
open Aornota.BridgeSim.Domain.Simulation.ComparisonConstraint

(* TODO-NNB...
type PartnershipShapeConstraint =
    | SuitCounts of (int * int * int * int) list
    | SuitConstraints of ComparisonConstraint<int> option * ComparisonConstraint<int> option * ComparisonConstraint<int> option * ComparisonConstraint<int> option *)

(* TODO-NNB...
type PartnershipScenario = private {
    Partnership: Partnership
    HcpConstraint: ComparisonConstraint<int<hcp>> option
    CcConstraint: ComparisonConstraint<int<cc>> option
    ShapeConstraint: PartnershipShapeConstraint option
    //Cards: Card list
    CustomPredicate: (Hand * Hand -> bool) option } *)

let [<Literal>] private MIN_PARTNERSHIP_HCP = 0<hcp>
let [<Literal>] private MAX_PARTNERSHIP_HCP = 40<hcp>
let [<Literal>] private MIN_PARTNERSHIP_CC = 0<cc>
let [<Literal>] private MAX_PARTNERSHIP_CC = 12<cc>
let [<Literal>] private MIN_PARTNERSHIP_SUIT = 0
let [<Literal>] private MAX_PARTNERSHIP_SUIT = 26
