module day17tests

open NUnit.Framework
open day17

[<TestFixture>]
type Day17Tests() = 

    [<Test>]
    member this.``This is true`` () = Assert.IsTrue true
