module day21tests

open NUnit.Framework
open day21

[<TestFixture>]
type Day21Tests() = 

    let swapPositionSample = "swap position 4 with position 5"
    let swapLetterSample = "swap letter a with letter b"
    let rotateLeftSample = "rotate left 4 steps"
    let rotateRightSample = "rotate right 4 steps"
    let rotatePositionSample = "rotate based on position of letter a"
    let reverseSample = "reverse positions 0 through 1"
    let moveSample = "move position 0 to position 1"

    let exampleData = "abcde" |> Seq.toArray
    let exampleInstructions = 
        [ "swap position 4 with position 0"
          "swap letter d with letter b"
          "reverse positions 0 through 4"
          "rotate left 1 step"
          "move position 1 to position 4"
          "move position 3 to position 0"
          "rotate based on position of letter b"
          "rotate based on position of letter d" ]
       |> List.map parseInstruction

    [<Test>]
    member this.``Applying 1 instruction on 'abcde' should return 'ebcda'`` () =
        let expected = "ebcda"
        let result = exampleInstructions |> List.take 1 
                     |> applyTransforms exampleData
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Applying 2 instructions on 'abcde' should return 'edcba'`` () =
        let expected = "edcba"
        let result = exampleInstructions |> List.take 2
                     |> applyTransforms exampleData
        Assert.That(result, Is.EqualTo expected)
    
    [<Test>]
    member this.``Applying 3 instructions on 'abcde' should return 'abcde'`` () =
        let expected = "abcde"
        let result = exampleInstructions |> List.take 3
                     |> applyTransforms exampleData
        Assert.That(result, Is.EqualTo expected)

    
    [<Test>]
    member this.``Applying 4 instructions on 'abcde' should return 'bcdea'`` () =
        let expected = "bcdea"
        let result = exampleInstructions |> List.take 4
                     |> applyTransforms exampleData
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Applying 5 instructions on 'abcde' should return 'bdeac'`` () =
        let expected = "bdeac"
        let result = exampleInstructions |> List.take 5
                     |> applyTransforms exampleData
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Applying 6 instructions on 'abcde' should return 'abdec'`` () =
        let expected = "abdec"
        let result = exampleInstructions |> List.take 6
                     |> applyTransforms exampleData
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Applying 7 instructions on 'abcde' should return 'ecabd'`` () =
        let expected = "ecabd"
        let result = exampleInstructions |> List.take 7
                     |> applyTransforms exampleData
        Assert.That(result, Is.EqualTo expected)
        
    [<Test>]
    member this.``Applying all instructions on 'abcde' should return 'decab'`` () =
        let expected = "decab"
        let result = exampleInstructions
                     |> applyTransforms exampleData
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Parsing move position sample should return Move (0,1)`` () = 
        let expected = Move (0,1)
        let result = parseInstruction moveSample
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Parsing reverse position sample should return Reverse (0,1)`` () = 
        let expected = Reverse (0,1)
        let result = parseInstruction reverseSample
        Assert.That(result, Is.EqualTo expected)
    
    [<Test>]
    member this.``Parsing rotate position sample should return RotatePosition a`` () = 
        let expected = RotatePosition 'a'
        let result = parseInstruction rotatePositionSample
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Parsing rotate right sample should return Rotate -4`` () = 
        let expected = Rotate -4
        let result = parseInstruction rotateRightSample
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Parsing rotate left sample should return Rotate 4`` () = 
        let expected = Rotate 4
        let result = parseInstruction rotateLeftSample
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Parsing swap letter sample should return SwapLetter ('a','b')`` () = 
        let expected = SwapLetter ('a','b')
        let result = parseInstruction swapLetterSample
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member this.``Parsing swap position sample should return SwapPosition (4,5)`` () = 
        let expected = SwapPosition (4,5)
        let result = parseInstruction swapPositionSample
        Assert.That(result, Is.EqualTo expected)
