module day18tests

open NUnit.Framework
open day18

[<TestFixture>]
type Day18Tests() = 
    let trap = '^'
    let safe = '.'

    let floorExample = ".^^.^.^^^^" |> Seq.toArray
    let sampleRow = "..^^." |> Seq.toArray

    [<Datapoints>]
    member this.SafeData = [|[|safe;safe;safe|]; [|trap;trap;trap|]; [|trap;safe;trap|]|]
    
    [<Theory>]
    member this.``Other combinations of trap and safe should be safe`` test = 
        let expected = safe
        let result = test |> safeOrTrap
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Calculating the safe tiles in ten rows of sample floor should be 38`` () =
        let expected = 38
        let result = floorExample |> buildFloorOfRows 10 |> totalSafeTiles
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Given '..^^.' the when computing the next row should be '.^^^^'`` () =
        let expected = ".^^^^" |> Seq.toArray
        let result = sampleRow |> computeNextRow
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Given only the right tile is a trap should then be a trap`` () = 
        let expected = trap
        let result = [|safe; safe; trap|] |> safeOrTrap
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Given only the left tile is a trap should then be a trap`` () = 
        let expected = trap
        let result = [|trap; safe; safe|] |> safeOrTrap
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Given the center and right tiles are traps, but its left tile is not should then be a trap`` () = 
        let expected = trap
        let result = [|safe; trap; trap|] |> safeOrTrap
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Given the left and center tiles are traps, but its right tile is not should then be a trap`` () = 
        let expected = trap
        let result = [|trap; trap; safe|] |> safeOrTrap
        Assert.That(result, Is.EqualTo expected)
