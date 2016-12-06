module day05tests

open NUnit.Framework
open day05

[<Test>]
let ``First key for door id `abc` should be 1`` () =
    let expected = 1uy
    let result = "abc" |> calcKeyCode |> Seq.head
    Assert.That(result, Is.EqualTo expected)