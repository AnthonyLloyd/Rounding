namespace Program

open System
open Expecto
open FsCheck

module Gen =
    let fraction a b c =
        let r = float a + float b / (abs (float c) + 1.0)
        //printfn "%A" (a,b,c)
        r
    type RationalFloat = RationalFloat of float
    let rationalFloat =
        Gen.map3 fraction Arb.generate Arb.generate Arb.generate
        |> Arb.fromGen
        |> Arb.convert RationalFloat (fun (RationalFloat f) -> f)
    let floatArb = Arb.convert (NormalFloat.op_Explicit) (NormalFloat) Arb.from
    type Float01 = Float01 of float
    let float01Arb =
        let maxValue = float UInt64.MaxValue
        Arb.convert
            (fun (DoNotSize a) -> float a / maxValue |> Float01)
            (fun (Float01 f) -> f * maxValue + 0.5 |> uint64 |> DoNotSize)
            Arb.from
    let addToConfig config = {config with arbitrary = typeof<Float01>.DeclaringType::config.arbitrary}

[<AutoOpen>]
module Auto =
    let (==) a e = Expect.equal a e ""
    let (=~) a e = Expect.floatClose Accuracy.high a e ""
    let private config = Gen.addToConfig FsCheckConfig.defaultConfig
    let testProp name =
        testPropertyWithConfig config name
    let ptestProp name =
        ptestPropertyWithConfig config name
    let ftestProp stdgen name =
        ftestPropertyWithConfig stdgen config name

module Rounding =
    let inline private round (f:float) = int(if f<0.0 then f-0.5 else f+0.5)
    /// absolute error increase
    let inline private absError (n:int) wt (wi:float) (ni:int) (d:int) =
        if   (wt * float d / float n > 0.0 && wi <= wt * float ni / float n)
          || (wt * float d / float n < 0.0 && wi >= wt * float ni / float n) then abs(0.5 * wt / float n)
        elif wi < wt * float ni / float n then wi - wt * (float ni + float d * 0.5) / float n |> max 0.0
        else wt * (float ni + float d * 0.5) / float n - wi |> max 0.0
        // abs(wi-float(ni+d)) - abs(wi-float ni)
    /// relative error increase
    let inline private relError n wt (wi:float) (ni:int) (d:int) = absError n wt wi ni d / (max (abs wi) (abs(wt * 0.01 / float n)))
    let distribute n weights =
        let wt = Array.sum weights
        let f = float n / wt
        let ns = Array.map ((*)f >> round) weights
        let d = n-Array.sum ns
        for __ = 1 to abs d do
            let _,_,_,i = Seq.mapi2 (fun i wi ni -> absError n wt wi ni (sign d), relError n wt wi ni (sign d), -wi, i) weights ns |> Seq.min
            ns.[i] <- ns.[i] + sign d
        ns
    let distributePrint n weights =
        let wt = Array.sum weights
        let f = float n / wt
        let ns = Array.map ((*)f >> round) weights
        let d = n-Array.sum ns
        for __ = 1 to abs d do
            Seq.mapi2 (fun i wi ni -> absError n wt wi ni (sign d), relError n wt wi ni (sign d), -wi, i) weights ns |> Seq.sort |> Seq.toList |> printfn "%A"
            let _,_,_,i = Seq.mapi2 (fun i wi ni -> absError n wt wi ni (sign d), relError n wt wi ni (sign d), -wi, i) weights ns |> Seq.min
            ns.[i] <- ns.[i] + sign d
        printfn "%A" ns
        ns

module Tests =
    let weightsAreValid w = abs(Array.sum w)*float Int32.MaxValue > (Seq.map abs w |> Seq.max)
    let rounding =
        testList "rounding" [
            test "n zero" { Rounding.distribute 0 [|406.0;348.0;246.0;0.0|] == [|0;0;0;0|] }
            test "twitter" { Rounding.distribute 100 [|406.0;348.0;246.0;0.0|] == [|40;35;25;0|] }
            test "twitter n negative" { Rounding.distribute -100 [|406.0;348.0;246.0;0.0|] == [|-40;-35;-25;0|] }
            test "twitter ws negative" { Rounding.distribute 100 [|-406.0;-348.0;-246.0;-0.0|] == [|40;35;25;0|] }
            test "twitter both negative" { Rounding.distribute -100 [|-406.0;-348.0;-246.0;-0.0|] == [|-40;-35;-25;0|] }
            testProp "n total correctly" (fun (n:int) (w:Gen.RationalFloat NonEmptyArray) ->
                let w = Array.map (fun (Gen.RationalFloat f) -> f) w.Get
                if weightsAreValid w then
                    let ns = Rounding.distribute n w
                    Expect.equal (Array.sum ns) n "sum ns = n"
            )
            testProp "negative n returns negative of positive n" (fun (n:int) (w:Gen.RationalFloat NonEmptyArray) ->
                let w = Array.map (fun (Gen.RationalFloat f) -> f) w.Get
                if weightsAreValid w then
                    let n1 = Rounding.distribute -n w |> Array.map (~-)
                    let n2 = Rounding.distribute n w
                    Expect.equal n1 n2 "n1 = n2"
            )
            testProp "increase with weight" (fun (n:int) (w:Gen.RationalFloat NonEmptyArray) ->
                let w = Array.map (fun (Gen.RationalFloat f) -> f) w.Get
                if weightsAreValid w && n>0 && Seq.sum w > 0.0 then
                    Rounding.distribute n w
                    |> Seq.zip w
                    |> Seq.sort
                    |> Seq.map snd
                    |> Seq.pairwise
                    |> Seq.iter (fun (ni1,ni2) -> 
                        if ni1 > ni2 then Rounding.distributePrint n w |> ignore
                        Expect.isLessThanOrEqual ni1 ni2 "ni1 <= ni2")
            )
        ]


module Main =
    [<EntryPoint>]
    let main args =
        runTestsWithArgs defaultConfig args Tests.rounding

