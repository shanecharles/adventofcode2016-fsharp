module day05tests

open NUnit.Framework
open day05

[<Test>]
let ``Part 2 second key for door id `abc` should be 5`` () =
    let expected = (1uy, "5")
    let result = "abc" |> calcKeyCode2 3231929 |> Seq.head
    Assert.That(result, Is.EqualTo expected)

[<Test>]
let ``Part 1 first key for door id `abc` should be 1`` () =
    let expected = 1uy
    let result = "abc" |> calcKeyCode1 3231929 |> Seq.head
    Assert.That(result, Is.EqualTo expected)