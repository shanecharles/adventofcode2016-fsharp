module day18

let safeOrTrap = function
                 | [|'^';'^';'.'|]
                 | [|'.';'^';'^'|]
                 | [|'^';'.';'.'|]
                 | [|'.';'.';'^'|] -> '^'
                 | _               -> '.'

let computeNextRow row =
    seq {
        yield '.'
        yield! row
        yield '.' } 
    |> Seq.windowed 3 
    |> Seq.map safeOrTrap
    |> Seq.toArray

let buildFloorOfRows rowCount row = 
    {1 .. rowCount-1} |> Seq.scan (fun state _ -> computeNextRow state) row
    |> Seq.toArray

let totalSafeTiles = Array.map (Array.filter (fun t -> t = '.') >> Array.length)
                   >> Array.sum
let safeTilesInFloorOfRows rowCount = buildFloorOfRows rowCount >> totalSafeTiles

[<EntryPoint>]
let main argv =
    let input = argv.[0] |> System.IO.File.ReadLines |> Seq.head |> Seq.toArray

    input |> safeTilesInFloorOfRows 40
    |> printfn "Day 18 part 1 result: %d"

    input |> safeTilesInFloorOfRows 400000
    |> printfn "Day 18 part 2 result: %d"
    0 // return an integer exit code
