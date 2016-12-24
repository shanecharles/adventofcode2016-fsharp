module day20tests

open NUnit.Framework
open day20

[<TestFixture>]
type Day20Tests() = 
    [<Test>]
    member this.``Is True`` () = Assert.IsTrue true
