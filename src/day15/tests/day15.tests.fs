module day15tests

open NUnit.Framework
open day15

[<TestFixture>]
type day15tests() = 
    [<Test>]
    member this.``This is true`` () = Assert.IsTrue true
