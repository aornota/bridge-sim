module Aornota.BridgeSim.Build.Targets

open Helpers
open Fake.Core
open Fake.Core.TargetOperators
open Fake.IO

initializeContext ()

Target.create "run-dev-console" (fun _ ->
    createMissingAppSettingsForDevelopment devConsoleDir
    runDotNet "run" devConsoleDir)

Target.create "run-tests" (fun _ -> runDotNet "run -c Release" testsDir)

Target.create "help" (fun _ ->
    printfn "\nThese useful build targets can be run via 'dotnet run {target}':"
    printfn "\n\trun-dev-console -> builds and runs [Debug] dev-console"
    printfn "\n\trun-tests -> builds and runs [Release] tests"
    printfn "\n\thelp -> shows this list of build targets\n")

(* TODO-NMB: Reinstate?...
let private dependencies = [
    "run-tests" ==> "run-dev-console"
]
*)

let private runOrDefault args =
    try
        match args with
        | [| target |] -> Target.runOrDefault target
        | _ -> Target.runOrDefault "run-dev-console"
        0
    with exn ->
        printfn "%A" exn
        1

[<EntryPoint>]
let main args = runOrDefault args
