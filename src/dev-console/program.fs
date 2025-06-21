module Aornota.BridgeSim.DevConsole.Program

open Aornota.BridgeSim.Common.Console
open Aornota.BridgeSim.Common.IfDebug
open Aornota.BridgeSim.Common.SourcedLogger

open Giraffe.SerilogExtensions
open Microsoft.Extensions.Configuration
open Serilog
open System

let [<Literal>] private SOURCE = "DevConsole.Program"

// TODO-NMB: Investigate why Release not writing to ...\logs?...

let private configuration =
    ConfigurationBuilder()
        .AddJsonFile("appsettings.json", false)
#if DEBUG
        .AddJsonFile("appsettings.development.json", false)
#else
        .AddJsonFile("appsettings.production.json", false)
#endif
        .Build()

do Log.Logger <- LoggerConfiguration().ReadFrom.Configuration(configuration).Destructure.FSharpTypes().CreateLogger()

let private sourcedLogger = Log.Logger |> sourcedLogger SOURCE

let private mainAsync () = async {
    try
        writeNewLine "Running " ConsoleColor.Yellow
        write (ifDebug "Debug" "Release") ConsoleColor.DarkYellow
        write $" {SOURCE}.mainAsync\n" ConsoleColor.Yellow

        //Scratch.Core.deck ()
        //Scratch.Auction.auctionAndDiagram ()
        //Scratch.Auction.duplicateScoring ()
        //Scratch.Deal.dealAndDiagrams 5 false
        //Scratch.Deal.serialization ()

        //Scratch.Dds.dds 10 true
        //Scratch.Dds.forcingOpening1C 10 true
        //Scratch.Dds.forcingOpening1CAnd1HResponse 10 true
        //Scratch.Dds.forcingOpening1CAnd1SResponse 10 true
        //Scratch.Dds.forcingOpening2C 10 false
        //Scratch.DdsSimulation.run 20
        //Scratch.DdsSimulation.wip ()

        //Scratch.Classification.run 10000

        //Scratch.Scenario.TwoCANewHope.allStatistics 500000
        Scratch.Scenario.TwoCANewHope.forcingOpening2CStatistics Scratch.Scenario.TwoCANewHope.Basic 1000

        //Scratch.Scenario.FiveFourMajor.run (Scratch.Simulation.DisplayOnly true) true 10
        //Scratch.Scenario.FiveFourMajor.run Scratch.Simulation.Minimal true 5000
        //Scratch.Scenario.FiveFourMajor.run Scratch.Simulation.SaveOnly true 500

        //Scratch.Scenario.SixFourMajor.run (Scratch.Simulation.DisplayOnly true) true 10
        //Scratch.Scenario.SixFourMajor.run Scratch.Simulation.Minimal true 5000

        //Scratch.Scenario.NoStayman.run (Scratch.Simulation.DisplayOnly true) true 10
        //Scratch.Scenario.NoStayman.run Scratch.Simulation.Minimal true 5000

        //Scratch.Scenario.TwoNtInvitational.run (Scratch.Simulation.DisplayOnly true) true 10
        //Scratch.Scenario.TwoNtInvitational.run Scratch.Simulation.Minimal true 5000

    with | exn -> sourcedLogger.Error("Unexpected error:\n\t{errorMessage}", exn.Message)

    writeNewLine "\nPress any key to exit..." ConsoleColor.Yellow
    Console.ReadKey () |> ignore
    writeBlankLine ()
    return 0 }

[<EntryPoint>]
let main _ =
    async {
        do! Async.SwitchToThreadPool ()
        return! mainAsync ()
    } |> Async.RunSynchronously
