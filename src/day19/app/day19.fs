module day19

let firstRound elvesCount = match elvesCount % 2 with 
                            | 0 -> [|1 .. 2 .. elvesCount|]
                            | _ -> elvesCount :: [1 .. 2 .. (elvesCount-2)] |> Seq.toArray
let steal elves =
    elves |> Seq.chunkBySize 2
    |> Seq.fold (fun (rem,es) e -> match e with
                                   | [|e'|] -> (Some e', es)
                                   | es'    -> (rem, (es' |> Seq.head) :: es)) (None, [])
    |> (fun (rem, es) -> let es' = es |> List.rev
                         match rem with
                         | Some e -> e :: es'
                         | None   -> es')
    |> Seq.toArray

let part1 nElves =
    let rec loop = function
                   | [||]  -> None
                   | [|e|] -> Some e
                   | es    -> es |> steal |> loop
    loop (firstRound nElves)


[<EntryPoint>]
let main argv =
    let input = argv.[0] |> int
    let formatResult = function Some x -> string x
                              | _      -> "No answer provided"
    input |> part1
    |> formatResult
    |> printfn "Day 19 part 1 result: %s"
    0 // return an integer exit code
