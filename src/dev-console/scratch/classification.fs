module Aornota.BridgeSim.DevConsole.Scratch.Classification

open Aornota.BridgeSim.Common.Console
open Aornota.BridgeSim.Common.Mathy
open Aornota.BridgeSim.Dds.Interop.Dds
open Aornota.BridgeSim.DevConsole.Common
open Aornota.BridgeSim.DevConsole.Scratch.Dds
open Aornota.BridgeSim.DevConsole.Scratch.Simulation
open Aornota.BridgeSim.Domain.Auction
open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Evaluation.Core
open Aornota.BridgeSim.Domain.Formatting.Auction
open Aornota.BridgeSim.Domain.Formatting.Deal
open Aornota.BridgeSim.Domain.Random.Deal
open Aornota.BridgeSim.Domain.Scoring.Auction

open System
open System.IO

(* TODO-NMB:
     - Type for classification e.g. OneSuiter[Major|Minor] | TwoSuiter[Majors|Minors|EqualOrLongerMajor|LongerMinor] | ThreeSuiter[ShortMajor|ShortMinor] | Balanced...
     - Type for balanced HCP range, e.g. 0-5 | 6-9 | 10-11 | 12-14 | 15-17 | 18-19 | 20-22 | 23-24 | 25+...
     - Type for not-balanced HCP range, e.g. 0-5 | 6-10 | 11-12 | 13-15 | 16-19 | 20-22 | 23+...
     - Type for combined HCP range?...
     - Type fpr level-of-fit, e.g. 7-only | single-8 | single-9 | single-10+ | double-8 | double-9 | double-10+ | triple-8...
     - Type for Hand + partner Hand?...

     - Using random deals:
        -- Get all 4 sets of Hand + partner Hand from each deal...
        -- List.collect these...
        -- List.take desired number of hands...
        -- Classify...
        -- Output summary statistics, e.g. classification broken down by HCP range (or by combined HCP range? level-of-fit? &c.)... *)

let run count =
    if count = 0 then raise CountMustBeGreaterThanZeroException
    let countText = if count = 1 then "deal" else "deals"
    writeNewLine $"TODO-NMB: Classifying {count} hands:\n" ConsoleColor.Magenta
