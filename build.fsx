#r "paket: groupref build //"
#if !FAKE
// See https://github.com/ionide/ionide-vscode-fsharp/issues/839#issuecomment-396296095.
#r "netstandard"
#r "Facades/netstandard"
#endif

#load "./.fake/build.fsx/intellisense.fsx"

open System

open Fake.Core
open Fake.DotNet
open Fake.IO

let private devConsoleDir = Path.getFullName "./src/dev-console"

let private runDotNet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd String.Empty
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s." cmd workingDir

let private createMissingAppSettingsForDevelopment dir =
    let requiredSettings, productionSettings = Path.combine dir "appsettings.development.json", "appsettings.production.json"
    if not (File.exists requiredSettings) then
        Shell.copyFile requiredSettings (Path.combine dir productionSettings)
        Trace.traceImportant (sprintf "WARNING -> %s did not exist and has been copied from %s; it will most likely need to be modified" requiredSettings productionSettings)

Target.create "run-dev-console" (fun _ ->
    createMissingAppSettingsForDevelopment devConsoleDir
    runDotNet "run" devConsoleDir)

Target.create "help" (fun _ ->
    printfn "\nThese useful build targets can be run via 'dotnet fake build -t {target}':"
    printfn "\n\trun-dev-console -> builds and runs [Debug] dev-console"
    printfn "\n\thelp -> shows this list of build targets\n")

Target.runOrDefaultWithArguments "run-dev-console"
