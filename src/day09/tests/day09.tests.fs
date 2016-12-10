module day09tests

open day09
open NUnit.Framework

[<TestFixture>]
type day09tests() =
    let marker = "(1x5)"
    let sample1 = "X(8x2)(3x3)ABCY"

    [<Test>]
    member this.``Given 'X(8x2)(3x3)ABCY' the v2 decompressed length should be 20`` () =
        let expected = 20L
        let result = sample1 |> parseInput |> lengthV2
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Given 'ADVENT' the decompressed length should be 6`` () =
        let expected = 6L
        let result = "ADVENT" |> parseInput |> lengthV1
        Assert.That(result, Is.EqualTo expected) 

    [<Test>]
    member this.``Given 'A(1x5)BC' the decompressed length should be 7`` () =
        let expected = 7L
        let result = "A(1x5)BC" |> parseInput |> lengthV1
        Assert.That(result, Is.EqualTo expected) 

    [<Test>]
    member this.``Given '(6x1)(1x3)A' the decompressed length should be 6`` () =
        let expected = 6L
        let result = "(6x1)(1x3)A" |> parseInput |> lengthV1
        Assert.That(result, Is.EqualTo expected) 
    
    [<Test>]
    member this.``Given 'X(8x2)(3x3)ABCY' the decompressed length should be 18`` () =
        let expected = 18L
        let result = sample1 |> parseInput |> lengthV1
        Assert.That(result, Is.EqualTo expected) 
    
    [<Test>]
    member this.``Given 'A(2x2)BCD(2x2)EFG' the decompressed length should be 11`` () =
        let expected = 11L
        let result = "A(2x2)BCD(2x2)EFG" |> parseInput |> lengthV1
        Assert.That(result, Is.EqualTo expected) 
    
    [<Test>]
    member this.``Marker '(1x5)' should be converted to (1,5)`` () =
        let expected = (1L,5L)
        let result = day09.parseMarker marker 
        Assert.That(result, Is.EqualTo expected)
