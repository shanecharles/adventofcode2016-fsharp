module day04tests

open NUnit.Framework
open day04

let data1 = ("aaaaa-bbb-z-y-x", 123, "abxyz")

let data2 = ("a-b-c-d-e-f-g-h", 987, "abcde")
let data3 = ("not-a-real-room", 404, "oarel")

let data4 = ("totally-real-room", 200, "decoy")

let checksum (_,_,c) = c
let data (d,_,_) = d
let dataSector (d,s,_) = d,s

[<Test>]
let ``Increasing 'z' by one will roll to 'a'`` () =
    let expected = 'a' |> byte
    let result = 'z' |> byte |> nextChar
    Assert.That(result, Is.EqualTo expected)

[<Test>]
let ``Validating 'totally-real-room-200' with checksum 'decoy' should return None`` () =
    let expected = None
    let result = data4 |> validateData
    Assert.That(result, Is.EqualTo expected)

[<Test>]
let ``Validating 'not-a-real-room-404' with checksum 'oarel' should return Some data`` () =
    let expected = data3 |> dataSector |> Some
    let result = data3 |> validateData
    Assert.That(result, Is.EqualTo expected)

[<Test>]
let ``Validating 'a-b-c-d-e-f-g-h-987' and checksum 'abcde' should return Some data`` () =
    let expected = data2 |> dataSector |> Some
    let result = data2 |> day04.validateData
    Assert.That(result, Is.EqualTo expected)

[<Test>]
let ``Calculating 'a-b-c-d-e-f-g-h-987' should have a checksum of 'abcde'`` () =
    let expected = data2 |> checksum |> Seq.toArray
    let result = data2 |> data |> calcChecksum
    Assert.That(result, Is.EqualTo expected)

[<Test>]
let ``Calculating 'aaaaa-bbb-z-y-x-123' should have a checksum of 'abxyz'`` () =
    let expected = data1 |> checksum |> Seq.toArray
    let result = data1 |> data |> calcChecksum
    Assert.That(result, Is.EqualTo expected)