module Rounding

open System
open Expecto
open FsCheck

let distribute n weights =
    let wt = Array.sum weights
    if Array.isEmpty weights
        || Seq.maxBy abs weights * float(abs n+1) |> abs
           >= abs wt * float Int32.MaxValue then None
    else
        let inline round f = if f<0.0 then f-0.5 else f+0.5
        let ns = Array.map ((*)(float n / wt) >> round >> int) weights
        let inline absError wi ni d =
            let wc = 0.5 * float d * wt / float n
            let wni = float ni * wt / float n
            if wc > 0.0 then min (wni-wi) 0.0 + wc
            else min (wi-wni) 0.0 - wc
        let d = n - Array.sum ns
        for __ = 1 to abs d do
            let _,_,_,i =
                Seq.mapi2 (fun i wi ni ->
                    let absErr = absError wi ni (sign d)
                    let relErr = absErr / abs wi
                    let weight = if wt > 0.0 then -wi else wi
                    absErr, relErr, weight, i
                ) weights ns |> Seq.min
            ns.[i] <- ns.[i] + sign d
        Some ns

module Gen =
    type RationalFloats = RationalFloats of float[]
    let rationalFloats =
        let fraction a b c = float a + float b / (abs (float c) + 1.0)
        Gen.map3 fraction Arb.generate Arb.generate Arb.generate
        |> Gen.arrayOf
        |> Arb.fromGen
        |> Arb.convert RationalFloats (fun (RationalFloats f) -> f)

let private config = {
    FsCheckConfig.defaultConfig with
        arbitrary = typeof<Gen.RationalFloats>.DeclaringType::FsCheckConfig.defaultConfig.arbitrary
}
let testProp name = testPropertyWithConfig config name
let ptestProp name = ptestPropertyWithConfig config name
let ftestProp stdgen name = ftestPropertyWithConfig stdgen config name

let roundingTests =
    testList "rounding" [
        test "empty" {
            let r = distribute 1 [||]
            Expect.equal r None "empty"
        }
        test "n zero" {
            let r = distribute 0 [|406.0;348.0;246.0;0.0|]
            Expect.equal r (Some [|0;0;0;0|]) "zero"
        }
        test "twitter" {
            let r = distribute 100 [|406.0;348.0;246.0;0.0|]
            Expect.equal r (Some [|40;35;25;0|]) "40 etc"
        }
        test "twitter n negative" {
            let r = distribute -100 [|406.0;348.0;246.0;0.0|]
            Expect.equal r (Some [|-40;-35;-25;0|]) "-40 etc"
        }
        test "twitter weights negative" {
            let r = distribute 100 [|-406.0;-348.0;-246.0;-0.0|]
            Expect.equal r (Some [|40;35;25;0|]) "40 etc"
        }
        test "twitter both negative" {
            let r = distribute -100 [|-406.0;-348.0;-246.0;-0.0|]
            Expect.equal r (Some [|-40;-35;-25;0|]) "-40 etc"
        }
        test "twitter tricky" {
            let r = distribute 100 [|404.0;397.0;57.0;57.0;57.0;28.0|]
            Expect.equal r (Some [|40;39;6;6;6;3|]) "o no"
        }
        test "negative example" {
            let r1 = distribute 42 [|1.5;1.0;39.5;-1.0;1.0|]
            Expect.equal r1 (Some [|2;1;39;-1;1|]) "2 etc"
            let r2 = distribute -42 [|1.5;1.0;39.5;-1.0;1.0|]
            Expect.equal r2 (Some [|-2;-1;-39;1;-1|]) "-2 etc"
        }
        testProp "ni total correctly" (fun n (Gen.RationalFloats ws) ->
            distribute n ws
            |> Option.iter (fun ns -> Expect.equal (Array.sum ns) n "sum ns = n")
        )
        testProp "negative n returns opposite of positive n" (
            fun n (Gen.RationalFloats ws) ->
                let r1 = distribute -n ws |> Option.map (Array.map (~-))
                let r2 = distribute n ws
                Expect.equal r1 r2 "r1 = r2"
        )
        testProp "increase with weight" (fun n (Gen.RationalFloats ws) ->
            let d = if Seq.sum ws > 0.0 <> (n>0) then -1 else 1
            distribute n ws
            |> Option.iter (
                   Seq.map ((*)d)
                >> Seq.zip ws
                >> Seq.sort
                >> Seq.pairwise
                >> Seq.iter (fun (ni1,ni2) ->
                    Expect.isLessThanOrEqual ni1 ni2 "ni1 <= ni2")
            )
        )
        testProp "smallest error" (fun n (Gen.RationalFloats ws) changes ->
            distribute n ws
            |> Option.iter (fun ns ->
                let totalError ns =
                    let wt = Seq.sum ws
                    Seq.map2 (fun ni wi ->
                        float ni * wt / float n - wi |> abs
                    ) ns ws
                    |> Seq.sum
                let err1 = totalError ns
                let l = Array.length ns
                List.iter (fun (i,j) ->
                    ns.[abs i % l] <- ns.[abs i % l] - 1
                    ns.[abs j % l] <- ns.[abs j % l] + 1
                ) changes
                let err2 = totalError ns
                Expect.floatLessThanOrClose
                    Accuracy.veryHigh err1 err2 "err1 <= err2"
                )
        )
    ]

[<EntryPoint>]
let main args =
    runTestsWithArgs defaultConfig args roundingTests