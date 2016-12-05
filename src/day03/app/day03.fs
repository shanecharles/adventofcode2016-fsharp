module day03

let parseLine (line : string) = line.Split(' ') |> Array.filter (not << System.String.IsNullOrEmpty) 
                                |> Array.map (int)
                                |> function [|x; y; z|] -> Some (x, y, z)
                                          | _           -> None

let parseInput1 (input : string []) = 
    input |> Seq.choose parseLine

let parseInput2 (input : string []) =
    input |> Seq.choose parseLine
    |> Seq.fold (fun (col1, col2, col3) (x, y, z) -> 
                    (x :: col1, y :: col2, z ::col3)) ([],[],[]) 
    |> function (col1, col2, col3) -> [col1; col2; col3]
    |> Seq.collect List.rev
    |> Seq.chunkBySize 3
    |> Seq.map (fun ([|x; y; z|]) -> (x,y,z))


let possibleTriangle ((x, y, z) as tri : int * int * int) =
    let [mn; md; lg] = [x; y; z] |> List.sort
    if lg < (mn + md) then Some tri
    else None

[<EntryPoint>]
let main argv =
    let file = argv |> Seq.head
    let countPossibleTriangles = Seq.choose possibleTriangle >> Seq.length
    let input = System.IO.File.ReadAllLines(file)
    input |> parseInput1
    |> countPossibleTriangles
    |> printfn "Day 3 part 1 result: %i"

    input |> parseInput2
    |> countPossibleTriangles
    |> printfn "Day 3 part 2 result: %i"

    0 // return an integer exit code
