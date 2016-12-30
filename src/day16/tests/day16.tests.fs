module day16tests

open NUnit.Framework
open day16

[<TestFixture>]
type Day16Tests() = 

    [<Test>]
    member this.``Given 10000 when needing 20 and calling computeChecksum should then result in 01100`` () =
        let expected = "01100"
        let result = "10000" |> mapInputString |> expandData 20 |> computeChecksum |> Seq.toArray |> System.String.Concat
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Given 10000 when needing 20 and calling expandData should then result in 10000011110010000111`` () =
        let expected = "10000011110010000111"
        let result = "10000" |> mapInputString |> expandData 20 |> Seq.toArray |> System.String.Concat
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Given 110010110100 when calling computeChecksum should then result in 100`` () =
        let expected = [1uy; 0uy; 0uy]
        let result = "110010110100" |> mapInputString |> computeChecksum |> Seq.toList
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Given pairChecksum with [1;1] should return 1`` () =
        let expected = 1uy
        let result = [|1uy; 1uy|] |> pairChecksum
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Given pairChecksum with [0;1] should return 0`` () =
        let expected = 0uy
        let result = [|0uy; 1uy|] |> pairChecksum
        Assert.That(result, Is.EqualTo expected)
    
    [<Test>]
    member this.``Given pairChecksum with [0;0] should return 1`` () =
        let expected = 1uy
        let result = [|0uy; 0uy|] |> pairChecksum
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Given pairWise with [1;0] should return 0`` () =
        let expected = 0uy
        let result = [|1uy; 0uy|] |> pairChecksum
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Given curveData with [1] should return 3 for result size`` () =
        let expected = 3
        let result  = [|1uy|] |> curveData
        Assert.That(result.Length, Is.EqualTo expected)
    
    [<Test>]
    member this.``Given curveData with [1] should return [1;0;0]`` () =
        let expected = [|1uy; 0uy; 0uy|]
        let result  = [|1uy|] |> curveData
        Assert.That(result |> Seq.toList, Is.EqualTo expected)

    [<Test>]
    member this.``Given flipBitsAndReverse [1;0;1] should result in [0;1;0]`` () =
        let expected = [|0uy; 1uy; 0uy|]
        let result = [|1uy; 0uy; 1uy|] |> flipBitsAndReverse
        Assert.That(result, Is.EqualTo expected)
    
    [<Test>]
    member this.``Given flipBitsAndReverse [1;1;1] should result in [0;0;0]`` () =
        let expected = [|0uy; 0uy; 0uy|]
        let result = [|1uy; 1uy; 1uy|] |> flipBitsAndReverse
        Assert.That(result, Is.EqualTo expected)
    
    [<Test>]
    member this.``Given flipBit 0 should result in 1`` () = 
        let expected = 1uy
        let result = flipBit 0uy
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Given flipBit 1 should result in 0`` () = 
        let expected = 0uy
        let result = flipBit 1uy
        Assert.That(result, Is.EqualTo expected)
