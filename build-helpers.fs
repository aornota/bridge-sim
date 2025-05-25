module Aornota.BridgeSim.Build.Helpers

open Fake.Core
open Fake.DotNet
open Fake.IO
open System

let devConsoleDir = Path.getFullName "./src/dev-console"
let testsDir = Path.getFullName "./src/tests"

let runDotNet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd String.Empty
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s." cmd workingDir

let createMissingAppSettingsForDevelopment dir =
    let requiredSettings, productionSettings = Path.combine dir "appsettings.development.json", "appsettings.production.json"
    if not (File.exists requiredSettings) then
        Shell.copyFile requiredSettings (Path.combine dir productionSettings)
        Trace.traceImportant (sprintf "WARNING -> %s did not exist and has been copied from %s; it will most likely need to be modified" requiredSettings productionSettings)

let initializeContext () =
    let execContext = Context.FakeExecutionContext.Create false "build.fsx" []
    Context.setExecutionContext (Context.RuntimeContext.Fake execContext)
