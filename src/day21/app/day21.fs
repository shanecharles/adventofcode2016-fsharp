module day21

open System.IO
open System.Text.RegularExpressions

type Instruction = 
    | SwapPosition of int * int
    | SwapLetter of char * char
    | Rotate of int
    | RotatePosition of char
    | Reverse of int * int
    | Move of int * int

let (|RegexMatch|_|) pattern input = 
    let matches = Regex.Match(input, pattern)
    if matches.Success
    then [ for g in matches.Groups -> g.Value ] |> List.tail |> Some
    else None

let setDirection x = function "left" -> x 
                            | _      -> -x

let parseInstruction = function
    | RegexMatch "swap position (\d+) with position (\d+)" [x; y] ->
        SwapPosition (int x, int y)
    | RegexMatch "swap letter (\w) with letter (\w)" [x; y;]      ->
        SwapLetter (Seq.head x, Seq.head y)
    | RegexMatch "rotate (left|right) (\d+) steps?" [dir; x]       ->
        Rotate <| setDirection (int x) dir
    | RegexMatch "rotate based on position of letter (\w)" [x]    ->
        RotatePosition <| Seq.head x
    | RegexMatch "reverse positions (\d+) through (\d+)" [x; y]   ->
        Reverse (int x, int y)
    | RegexMatch "move position (\d+) to position (\d+)" [x; y]   ->
        Move (int x, int y)
    | ins -> failwith <| sprintf "Invalid input instruction: %s" ins

let swapPosition x y (data : char []) = 
    let valX = data.[x]
    let valY = data.[y]
    seq { for i in 0 .. data.Length-1 do
            yield if i = x then valY
                  elif i = y then valX
                  else data.[i] }
    |> Seq.toArray

let swapLetters x y (data : char []) =
    data |> Array.map (function c when c = x -> y
                              | c when c = y -> x
                              | c            -> c)

let rotate x (data : char []) =
    let x' = x % data.Length
    if x' < 0 then 
        let limit = (data.Length + x')
        Array.append data.[limit..] data.[0 .. limit-1]
    else
        Array.append data.[x'..] data.[0 .. x'-1]

let rotatePosition x (data : char []) =
    let y = data |> Array.findIndex ((=) x)
    rotate -(y + 1 + if y >= 4 then 1 else 0) data

let reversePositions x y (data : char []) =
    let hi = max x y
    let low = min x y
    seq { if low > 0 then yield! data.[0..low-1] 
          yield! (data.[low..hi] |> Array.rev)
          if hi < (data.Length-1) then yield! data.[hi+1..] }
    |> Seq.toArray

let movePosition x y (data : char []) =
    let c = data.[x]
    seq {
        for i in 0 .. data.Length-1 do
            if i = y && x < y then yield data.[i]; yield c
            elif i = y && x > y then yield c; yield data.[i]
            elif i <> x then yield data.[i] }
    |> Seq.toArray 

let applyInstruction data = 
    function
    | SwapLetter (x, y)   -> swapLetters x y
    | SwapPosition (x, y) -> swapPosition x y
    | Move (x, y)         -> movePosition x y
    | Rotate x            -> rotate x
    | RotatePosition x    -> rotatePosition x
    | Reverse (x,y)       -> reversePositions x y
    >> function f -> f data

let applyTransforms data = Seq.fold applyInstruction data
                           >> System.String.Concat

let scanTransforms data = Seq.scan applyInstruction data
let parseInputFile = File.ReadAllLines >> Array.map parseInstruction

[<EntryPoint>]
let main argv =
    let input = argv.[0] |> parseInputFile
    let data = [|'a'..'h'|]
    input |> applyTransforms data
    |> printfn "Day 21 part 1 result: %s"
    0 // return an integer exit code
