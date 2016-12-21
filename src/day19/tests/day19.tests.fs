module day19tests

open NUnit.Framework
open day19

[<TestFixture>]
type Day19Tests() = 
    [<Test>]
    member this.``Is true`` () = Assert.IsTrue true
