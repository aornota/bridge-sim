module Aornota.BridgeSim.Common.Mathy

open System
open System.Security.Cryptography

type Mean<[<Measure>] 'u> = {
    Total : int<'u>
    Count : int }
    with
    static member Create(total, count) =
        if count <= 0 then failwith $"{(nameof count)} must be greater than zero"
        { Total = total ; Count = count }
    static member FromList(list) = Mean<_>.Create(list |> List.sum, list.Length)
    static member Combine(mean1, mean2) = { Total = mean1.Total + mean2.Total ; Count = mean1.Count + mean2.Count }
    static member Update(mean, value) = { Total = mean.Total + value ; Count = mean.Count + 1 }
    member this.Mean : float<'u> = LanguagePrimitives.FloatWithMeasure (float this.Total / float this.Count)

let randoms count =
    use rng = new RNGCryptoServiceProvider()
    let bytes = Array.create 4 0uy
    [
        for _ in 1..count do
            rng.GetBytes(bytes)
            BitConverter.ToInt32(bytes, 0)
    ]

let normalizedRandom () = abs (float (randoms 1 |> List.head) / float Int32.MaxValue)

// See Tomas Petricek's answer to https://stackoverflow.com/questions/4495597/combinations-and-permutations-in-f.
let rec combinations acc size list = seq {
    match size, list with
    | n, h :: t ->
        if n > 0 then yield! combinations (h :: acc) (n - 1) t
        if n >= 0 then yield! combinations acc n t
    | 0, [] -> yield acc
    | _, [] -> () }

