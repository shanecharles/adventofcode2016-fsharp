module day03tests

open day03
open NUnit.Framework

[<TestFixture>]
type Day03Tests () =

    [<Datapoints>]
    member x.Triangles = [| (5,10,25)
                            (5,25,10)
                            (10,25,5)
                            (10,5,25)
                            (25,10,5)
                            (25,5,10) |]
    
    [<Theory>]
    member x.``A triangle with sides of 5, 10, and 25 passed to possibleTriangle should result with None`` (triangle : int * int * int) =
        let expected = None
        let result = triangle |> possibleTriangle
        Assert.That(result, Is.EqualTo(expected))

