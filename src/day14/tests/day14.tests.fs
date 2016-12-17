module day14tests

open NUnit.Framework
open day14

[<TestFixture>]
type Day14Tests() = 

    [<Test>]
    member this.``This is true`` () = Assert.IsTrue true
