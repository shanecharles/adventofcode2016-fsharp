module day03

let parseInput (input : string []) = 
    input |> Seq.map (fun line -> line.Split(' ') |> Array.filter (not << System.String.IsNullOrEmpty) 
                                  |> Array.map (int)
                                  |> function [|x; y; z|] -> (x, y, z))

let possibleTriangle ((x, y, z) as tri : int * int * int) =
    let [mn; md; lg] = [x; y; z] |> List.sort
    if lg < (mn + md) then Some tri
    else None

[<EntryPoint>]
let main argv =
    let file = argv |> Seq.head
    let triangles = System.IO.File.ReadAllLines(file) |> parseInput
    triangles |> Seq.choose possibleTriangle 
    |> Seq.length
    |> printfn "Day 3 part 1 result: %i" 
    0 // return an integer exit code
