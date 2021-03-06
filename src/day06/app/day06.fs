module day06

open System.Collections.Generic
let addTo (d : Dictionary<int, char list>) i c = match d.ContainsKey(i) with
                                                 | true -> d.[i] <- c :: d.[i]
                                                 | _    -> d.[i] <- [c]

let part1Sorter : (char * char seq) seq ->  (char * char seq) seq = 
    Seq.sortByDescending (fun (_, cs) -> cs |> Seq.length)

let part2Sorter : (char * char seq) seq ->  (char * char seq) seq = 
    Seq.sortBy (fun (_, cs) -> cs |> Seq.length)

let decodeSignal sorter lines : char [] =
    let dict = new Dictionary<int, char list>()
    let addTo' = addTo dict
    lines |> Seq.iter (Seq.iteri addTo')
    dict |> Seq.map (fun (kv) -> kv.Key,kv.Value)
    |> Seq.sort
    |> Seq.map (snd >> Seq.groupBy id >> sorter >> Seq.head >> fst)
    |> Seq.toArray
    

[<EntryPoint>]
let main argv =
    let file = argv |> Seq.head
    let signals = System.IO.File.ReadAllLines(file)
    signals |> decodeSignal part1Sorter |> System.String.Concat 
    |> printfn "Day 6 part 1 result: %s"
    signals |> decodeSignal part2Sorter |> System.String.Concat 
    |> printfn "Day 6 part 2 result: %s"
    0 // return an integer exit code
