module day01Tests

open NUnit.Framework
open day01

[<TestFixture>]
type Day01Tests () =
    let sample1 = "R2, L3"
    let sample2 = "R2, R2, R2"
    let sample3 = "R5, L5, R5, R3"

    let part2_sample = "R8, R4, R4, R8"

    
    [<Test>]
    member x.``Running part2 sample for part 2 should equal 4 blocks away.`` () =
        let expected = 4
        let result = part2_sample |> mapInputString |> day1Solution2
        Assert.That(result, Is.EqualTo(expected))

    [<Test>]
    member x.``Running sample1 for part 1 should equal 5 blocks away.`` () =
        let expected = 5
        let result = sample1 |> mapInputString |> day1Solution1
        Assert.That(result, Is.EqualTo(expected))

    [<Test>]
    member x.``Running sample2 for part 1 should equal 2 blocks away.`` () =
        let expected = 2
        let result = sample2 |> mapInputString |> day1Solution1
        Assert.That(result, Is.EqualTo(expected))

    [<Test>]
    member x.``Running sample3 for part 1 should equal 12 blocks away.`` () =
        let expected = 12
        let result = sample3 |> mapInputString |> day1Solution1
        Assert.That(result, Is.EqualTo(expected))