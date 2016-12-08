module day08tests

open day08
open NUnit.Framework

[<TestFixture>]
type Day08Tests () =

    [<Test>]
    member this.``A display with should be able to turn on a 3x3 section`` () =
        let sut = Display(3,3)
        sut.TurnOnSection(3,3)
        let results = sut.Display.[0..2,0..2] |> Seq.cast<int> |> Seq.forall isLit
        Assert.IsTrue(results)
        
    [<Test>]
    member this.``Rotating a column 3 of a display of 1,6 with first 3 lit should have last 3 lit`` () =
        let sut = Display(1,6)
        sut.TurnOnSection(1,3)
        sut.RotateColumn(0,3)
        Assert.IsTrue(sut.Display.[0,3..5] |> Seq.cast<int> |> Seq.forall isLit )

    [<Test>]
    member this.``Rotating a column 3 of a display of 1,6 with first 3 lit should have first 3 not lit`` () =
        let sut = Display(1,6)
        sut.TurnOnSection(1,3)
        sut.RotateColumn(0,3)
        Assert.IsFalse(sut.Display.[0,0..2] |> Seq.cast<int> |> Seq.forall isLit)