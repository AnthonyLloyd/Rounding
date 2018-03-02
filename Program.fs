﻿module Program

open System
open Expecto
open FsCheck

module Gen =
    type RationalFloat = RationalFloat of float
    let rationalFloat =
        let fraction a b c = float a + float b / (abs (float c) + 1.0)
        Gen.map3 fraction Arb.generate Arb.generate Arb.generate
        |> Arb.fromGen
        |> Arb.convert RationalFloat (fun (RationalFloat f) -> f)
    //let floatArb = Arb.convert (NormalFloat.op_Explicit) (NormalFloat) Arb.from
    let addToConfig config = {config with arbitrary = typeof<RationalFloat>.DeclaringType::config.arbitrary}

let private config = Gen.addToConfig FsCheckConfig.defaultConfig
let testProp name = testPropertyWithConfig config name
let ptestProp name = ptestPropertyWithConfig config name
let ftestProp stdgen name = ftestPropertyWithConfig stdgen config name

module Rounding =
    let inline private round (f:float) = int(if f<0.0 then f-0.5 else f+0.5)
    
    let inline private absError (n:int) (wt:float) (wi:float) (ni:int) (d:int) =
        let wc = wt * float d / float n
        let wni = wt * float ni / float n
        if (wc > 0.0 && wi <= wni) || (wc < 0.0 && wi >= wni) then abs(0.5 * wc)
        elif wi < wni then wi - wni  - 0.5 * wc |> max 0.0
        else wni + 0.5 * wc - wi |> max 0.0

    let inline private relError n wt wi ni d =
        absError n wt wi ni d / abs wi

    let distribute n weights =
        let wt = Array.sum weights
        if abs(Seq.maxBy abs weights * float(abs n+1)) >= abs wt * float Int32.MaxValue then None
        else
            let ns = Array.map ((*)(float n / wt) >> round) weights
            let d = n - Array.sum ns
            for __ = 1 to abs d do
                let _,_,_,i = Seq.mapi2 (fun i wi ni ->
                                            let e1 = absError n wt wi ni (sign d)
                                            let e2 = relError n wt wi ni (sign d)
                                            let e3 = if wt > 0.0 then -wi else wi
                                            e1, e2, e3, i
                                        ) weights ns |> Seq.min
                ns.[i] <- ns.[i] + sign d
            Some ns

    let distributePrint n weights =
        let wt = Array.sum weights
        if abs(Seq.maxBy abs weights * float(abs n+1)) >= abs wt * float Int32.MaxValue then None
        else
            let f = float n / wt
            let ns = Array.map ((*)f >> round) weights
            let d = n - Array.sum ns
            for __ = 1 to abs d do
                Seq.mapi2 (fun i wi ni ->
                                            let e1 = absError n wt wi ni (sign d)
                                            let e2 = relError n wt wi ni (sign d)
                                            let e3 = if wt > 0.0 then -wi else wi
                                            e1, e2, e3, i
                                        ) weights ns
                |> Seq.sort |> Seq.toList |> printfn "%A"                            

                let _,_,_,i = Seq.mapi2 (fun i wi ni ->
                                            let e1 = absError n wt wi ni (sign d)
                                            let e2 = relError n wt wi ni (sign d)
                                            let e3 = if wt > 0.0 then -wi else wi
                                            e1, e2, e3, i
                                        ) weights ns |> Seq.min
                ns.[i] <- ns.[i] + sign d
            Some ns        
let roundingTests =
    testList "rounding" [
        test "n zero" {
            let r = Rounding.distribute 0 [|406.0;348.0;246.0;0.0|]
            Expect.equal r (Some [|0;0;0;0|]) "zero"
        }
        test "twitter" {
            let r = Rounding.distribute 100 [|406.0;348.0;246.0;0.0|]
            Expect.equal r (Some [|40;35;25;0|]) "40 etc"
        }
        test "twitter n negative" {
            let r = Rounding.distribute -100 [|406.0;348.0;246.0;0.0|]
            Expect.equal r (Some [|-40;-35;-25;0|]) "-40 etc"
        }
        test "twitter ws negative" {
            let r = Rounding.distribute 100 [|-406.0;-348.0;-246.0;-0.0|]
            Expect.equal r (Some [|40;35;25;0|]) "40 etc"
        }
        test "twitter both negative" {
            let r = Rounding.distribute -100 [|-406.0;-348.0;-246.0;-0.0|]
            Expect.equal r (Some [|-40;-35;-25;0|]) "-40 etc"
        }
        test "twitter tricky" {
            let r = Rounding.distribute 100 [|404.0;397.0;47.0;47.0;47.0;58.0|]
            Expect.equal r (Some [|40;39;5;5;5;6|]) "o no"
        }
        test "negative example" {
            let r1 = Rounding.distribute 42 [|1.5;1.0;39.5;-1.0;1.0|]
            Expect.equal r1 (Some [|2;1;39;-1;1|]) "2 etc"
            let r2 = Rounding.distribute -42 [|1.5;1.0;39.5;-1.0;1.0|]
            Expect.equal r2 (Some [|-2;-1;-39;1;-1|]) "-2 etc"
        }
        testProp "n total correctly" (
            fun (n:int) (w:Gen.RationalFloat NonEmptyArray) ->
                let w = Array.map (fun (Gen.RationalFloat f) -> f) w.Get
                Rounding.distribute n w
                |> Option.iter (fun ns -> Expect.equal (Array.sum ns) n "sum ns = n")
        )
        testProp "negative n returns negative of positive n" (
            fun (n:int) (w:Gen.RationalFloat NonEmptyArray) ->
                let w = Array.map (fun (Gen.RationalFloat f) -> f) w.Get
                let r1 = Rounding.distribute -n w |> Option.map (Array.map (~-))
                let r2 = Rounding.distribute n w
                Expect.equal r1 r2 "r1 = r2"
        )
        testProp "increase with weight" (
            fun (n:int) (w:Gen.RationalFloat NonEmptyArray) ->
                let w = Array.map (fun (Gen.RationalFloat f) -> f) w.Get
                let d = if Seq.sum w > 0.0 <> (n>0) then -1 else 1
                Rounding.distribute n w
                |> Option.iter (
                       Seq.map ((*)d)
                    >> Seq.zip w
                    >> Seq.sort
                    >> Seq.pairwise
                    >> Seq.iter (fun (ni1,ni2) ->
                        Expect.isLessThanOrEqual ni1 ni2 "ni1 <= ni2")
                )
        )
    ]

[<EntryPoint>]
let main args =
    runTestsWithArgs defaultConfig args roundingTests

