module Aornota.BridgeSim.Tests.Program

open Aornota.BridgeSim.Common.Console
open Aornota.BridgeSim.Common.IfDebug
open Aornota.BridgeSim.Common.SourcedLogger

open Expecto
open Giraffe.SerilogExtensions
open Microsoft.Extensions.Configuration
open Serilog
open System

let [<Literal>] private SOURCE = "Tests.Program"

let private configuration =
    ConfigurationBuilder()
        .AddJsonFile("appsettings.json", false)
        .Build()

do Log.Logger <- LoggerConfiguration().ReadFrom.Configuration(configuration).Destructure.FSharpTypes().CreateLogger()

let private sourcedLogger = sourcedLogger SOURCE Log.Logger

let private mainAsync () = async {
    writeNewLine "Running " ConsoleColor.Yellow
    write (ifDebug "Debug" "Release") ConsoleColor.DarkYellow
    write $" {SOURCE}.mainAsync\n" ConsoleColor.Yellow

    let mutable retval = 0

    let updateRetval fRunTests =
        let retval' = fRunTests ()
        retval <- max retval retval'

    try
        writeNewLine "duplicateScoringTests:\n" ConsoleColor.Magenta
        updateRetval (fun _ -> runTestsWithCLIArgs [] [||] Tests.duplicateScoringTests)

        if retval = 1 then
            writeBlankLine()
            sourcedLogger.Error "One or more tests failed"
    with | exn ->
        sourcedLogger.Error ("Unexpected error: {errorMessage}\n{stackTrace}", exn.Message, exn.StackTrace)
        retval <- 1

    writeBlankLine()
    return retval }

[<EntryPoint>]
let main _ =
    async {
        do! Async.SwitchToThreadPool()
        return! mainAsync ()
    } |> Async.RunSynchronously
