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
let ``Decoding the sample data with part 1 should result with 'easter'`` () =
    let expected = "easter" |> Seq.toArray
    let result = sampleData |> day06.decodeSignal day06.part1Sorter
    Assert.That(result, Is.EqualTo expected)

[<Test>]
let ``Decoding the sample data with part 2 should result with 'advent'`` () =
    let expected = "advent" |> Seq.toArray
    let result = sampleData |> day06.decodeSignal day06.part2Sorter
    Assert.That(result, Is.EqualTo expected)