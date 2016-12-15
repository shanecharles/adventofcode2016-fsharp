module day13tests

open NUnit.Framework
open day13

[<TestFixture>]
type day13tests() = 

    [<Test>]
    member this.``This is true`` () = Assert.IsTrue(true)
