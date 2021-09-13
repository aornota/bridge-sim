module Aornota.BridgeSim.Dds.Interop.Dds

open Aornota.BridgeSim.Domain.Auction
open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Deal
open Aornota.BridgeSim.Domain.Evaluation.Core
open Aornota.BridgeSim.Domain.Formatting.Core

open System.Runtime.InteropServices

type private DealPbn = char array
type private Results = int array

[<DllImport("dds.dll")>]
extern int private CalcDDtablePBN([<param:In; MarshalAs(UnmanagedType.LPArray, SizeConst = 80)>]DealPbn dealPbn, [<param:Out; MarshalAs(UnmanagedType.LPArray, SizeConst = 20)>]Results results)

// TODO-NMB: Par/DealerPar?...

exception CalculateDoubleDummyException of string
exception NoDoubleDummyResultForPositionAndStrainException of Position * Strain

type DoubleDummyResults = private { Results' : (Position * Strain * uint) list } with
    static member Make(raw:int array) =
        let positionOffset = function | North -> 0 | East -> 1 | South -> 2 | West -> 3
        let strainOffset = function | Suit Spade -> 0 | Suit Heart -> 4 | Suit Diamond -> 8 | Suit Club -> 12 | NoTrump -> 16
        let results =
            [ North ; East ; South ; West ]
            |> List.collect (fun position -> [ Suit Club ; Suit Diamond ; Suit Heart ; Suit Spade ; NoTrump ] |> List.map (fun strain -> position, strain))
            |> List.map (fun (position, strain) -> position, strain, uint (raw.[positionOffset position + strainOffset strain]))
        { Results' = results }
    member this.Tricks(position, strain) =
        match this.Results' |> List.tryFind (fun (position', strain', _) -> position' = position && strain' = strain) with
        | Some (_, _, tricks) -> tricks
        | None -> raise (NoDoubleDummyResultForPositionAndStrainException (position, strain))
    member this.Level(position, strain) =
        match this.Tricks(position, strain) with
        | 13u -> Some SevenLevel | 12u -> Some SixLevel | 11u -> Some FiveLevel | 10u -> Some FourLevel | 9u -> Some ThreeLevel | 8u -> Some TwoLevel | 7u -> Some OneLevel
        | _ -> None

let private dealPbn (deal:Deal) =
    let handPbn (hand:Hand) =
        let suitPbn (suit:Suit) = hand.CardsForSuit(suit) |> List.map (fun card -> card.Rank.ShortText) |> String.concat ""
        $"{suitPbn Spade}.{suitPbn Heart}.{suitPbn Diamond}.{suitPbn Club}"
    let northHand, eastHand, southHand, westHand = deal.Hand(North), deal.Hand(East), deal.Hand(South), deal.Hand(West)
    let pbn = Array.zeroCreate 80
    $"{North.ShortText}:{handPbn northHand} {handPbn eastHand} {handPbn southHand} {handPbn westHand}" |> String.iteri (fun i char -> pbn.[i] <- char)
    pbn

let calculateDoubleDummy deal =
    let results = Array.zeroCreate 20
    try
        CalcDDtablePBN(dealPbn deal, results) |> ignore
    with | exn -> raise (CalculateDoubleDummyException exn.Message)
    DoubleDummyResults.Make(results)
