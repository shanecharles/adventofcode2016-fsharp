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

let cross elves =
    let rec loop index = function
        | [| e |] -> e
        | es      -> 
            let mid = es.Length / 2
            let nth = (index + mid) % es.Length
            es.[nth] <- -1
            let es' = es |> Array.filter ((<>) -1)
            let index' = if es'.Length <= index then 0
                         elif nth <= index then index
                         else index + 1
            loop index' es'
    loop 0 elves

let part2 elveCount = 
    let l = log (float elveCount) / log 3.0 |> floor |> int
    let n = pown 3 l
    let m = elveCount - n
    if m = 0 then n
    elif m <= n then m
    else n + (m - n) * 2


[<EntryPoint>]
let main argv =
    let input = argv.[0] |> int
    let formatResult = function Some x -> string x
                              | _      -> "No answer provided"
    input |> part1
    |> formatResult
    |> printfn "Day 19 part 1 result: %s"

    input |> part2
    |> printfn "Day 19 part 2 result: %d"
    0 // return an integer exit code
