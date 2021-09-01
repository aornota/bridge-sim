module Aornota.BridgeSim.Tests.Tests

open Aornota.BridgeSim.Domain.Auction
open Aornota.BridgeSim.Domain.Core
open Aornota.BridgeSim.Domain.Scoring.Auction
open Aornota.BridgeSim.Domain.Formatting.Auction
open Aornota.BridgeSim.Domain.Formatting.Core

open Expecto

// TODO-NMB: More tests...

let [<Tests>] duplicateScoringTests =
    let assertContractScore level strain stakes (vulnerability:Vulnerability) tricksTaken expectedScore =
        let contract = Contract (level, strain, stakes, South)
        let message = $"""{level.ShortText}{strain.ShortText}{stakes.ShortText} when {vulnerability.TextLower} taking {tricksTaken} {if tricksTaken = 1u then "trick" else "tricks"} did not score {expectedScore}"""
        Expect.equal (contract.DuplicateScore(Some (vulnerability, tricksTaken))) expectedScore message
    testList "Duplicate scoring tests" [
        test "DuplicateScore should throw MustProvideVulnerabilityAndTricksTakenForContractException when not provided for Contract" {
            Expect.throwsT<MustProvideVulnerabilityAndTricksTakenForContractException>
                (fun _ -> (Contract (OneLevel, Suit Club, Undoubled, South)).DuplicateScore(None) |> ignore) "DuplicateScore did not throw MustProvideVulnerabilityAndTricksTakenForContractException" }
        test "DuplicateScore should throw MustNotProvideVulnerabilityAndTricksTakenForPassedOutException when provided for PassedOut" {
            Expect.throwsT<MustNotProvideVulnerabilityAndTricksTakenForPassedOutException>
                (fun _ -> PassedOut.DuplicateScore(Some (NotVulnerable, 1u)) |> ignore) "DuplicateScore did not throw MustNotProvideVulnerabilityAndTricksTakenForPassedOutException" }
        test "Passed out should score 0" { Expect.equal (PassedOut.DuplicateScore(None)) 0 "Passed out did not score 0" }
        // TODO-NMB: Double-check that expected scores are correct...
        test "2NT non-vul. taking 8 tricks should score 120" { assertContractScore TwoLevel NoTrump Undoubled NotVulnerable 8u 120 }
        test "2NTx vul. taking 8 tricks should score 690" { assertContractScore TwoLevel NoTrump Doubled Vulnerable 8u 690 }
        test "2NT vul. taking 9 tricks should score 150" { assertContractScore TwoLevel NoTrump Undoubled Vulnerable 9u 150 }
        test "3NT non-vul. taking 9 tricks should score 400" { assertContractScore ThreeLevel NoTrump Undoubled NotVulnerable 9u 400 }
        test "3NT vul. taking 9 tricks should score 600" { assertContractScore ThreeLevel NoTrump Undoubled Vulnerable 9u 600 }
        test "3NT vul. taking 10 tricks should score 630" { assertContractScore ThreeLevel NoTrump Undoubled Vulnerable 10u 630 }
        test "2H non-vul. taking 9 tricks should score 140" { assertContractScore TwoLevel (Suit Heart) Undoubled NotVulnerable 9u 140 }
        test "2Sx non-vul. taking 8 tricks should score 470" { assertContractScore TwoLevel (Suit Spade) Doubled NotVulnerable 8u 470 }
        test "1Cxx vul. taking 10 tricks should score 1430" { assertContractScore OneLevel (Suit Club) Redoubled Vulnerable 10u 1430 }
        test "5D vul. taking 11 tricks should score 600" { assertContractScore FiveLevel (Suit Diamond) Undoubled Vulnerable 11u 600 }
        test "6NT non-vul. taking 12 tricks should score 990" { assertContractScore SixLevel NoTrump Undoubled NotVulnerable 12u 990 }
        test "6NTx vul. taking 13 tricks should score 1880" { assertContractScore SixLevel NoTrump Doubled Vulnerable 13u 1880 }
        test "7NT vul. taking 13 tricks should score 2220" { assertContractScore SevenLevel NoTrump Undoubled Vulnerable 13u 2220 }
        test "3NT non-vul. taking 4 tricks should score -250" { assertContractScore ThreeLevel NoTrump Undoubled NotVulnerable 4u -250 }
        test "3NTx non-vul. taking 4 tricks should score -1100" { assertContractScore ThreeLevel NoTrump Doubled NotVulnerable 4u -1100 }
        test "3NTxx non-vul. taking 4 tricks should score -2200" { assertContractScore ThreeLevel NoTrump Redoubled NotVulnerable 4u -2200 }
        test "3NTx vul. taking 5 tricks should score -1100" { assertContractScore ThreeLevel NoTrump Doubled Vulnerable 5u -1100 }
        test "3NTxx vul. taking 6 tricks should score -1600" { assertContractScore ThreeLevel NoTrump Redoubled Vulnerable 6u -1600 }
        // TODO-NMB: More duplicate scoring test cases...
    ]
