module day02tests

open NUnit.Framework
open day02

[<TestFixture>]
type Day02Tests () =
    let sample = ["ULL";"RRDDD";"LURDL";"UUUUD"]
    [<Test>]
    member x.``Running sample data on keypad1 should result with ['1';'9';'8';'5'].`` () =
        let expected = ['1';'9';'8';'5']
        let result = sample |> scanKeypad1
        Assert.That(result, Is.EqualTo(expected))

    [<Test>]
    member x.``Running sample data on keypad2 should result with ['5';'D';'B';'3'].`` () =
        let expected = ['5';'D';'B';'3']
        let result = sample |> scanKeypad2
        Assert.AreEqual(expected, result)