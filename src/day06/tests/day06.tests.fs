module day06tests

open NUnit.Framework

let sampleData = [  "eedadn"
                    "drvtee"
                    "eandsr"
                    "raavrd"
                    "atevrs"
                    "tsrnev"
                    "sdttsa"
                    "rasrtv"
                    "nssdts"
                    "ntnada"
                    "svetve"
                    "tesnvt"
                    "vntsnd"
                    "vrdear"
                    "dvrsen"
                    "enarar" ]

[<Test>]
let ``Sample lines decode of first character should be 'e'`` () =
    let expected = 'e'
    let result = sampleData |> Seq.collect (Seq.take 1) |> Seq.groupBy id |> Seq.sortByDescending (fun (_,cs) -> cs |> Seq.length)
                 |> Seq.map fst
                 |> Seq.head
    Assert.That(result, Is.EqualTo expected)

[<Test>]
let ``Decoding the sample data should result with 'easter'`` () =
    let expected = "easter"
    let result = sampleData |> day06.decodeSignal
    Assert.That(result, Is.EqualTo expected)

