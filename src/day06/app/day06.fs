module day06

open System.Collections.Generic
let addTo (d : Dictionary<int, char list>) i c = match d.ContainsKey(i) with
                                                 | true -> d.[i] <- c :: d.[i]
                                                 | _    -> d.[i] <- [c]

let decodeSignal lines =
    let dict = new Dictionary<int, char list>()
    let addTo' = addTo dict
    lines |> Seq.iter (Seq.iteri addTo')
    dict |> Seq.map (fun (kv) -> kv.Key,kv.Value)
    |> Seq.sort
    |> Seq.map (snd >> Seq.groupBy id >> (Seq.sortByDescending (fun (_, cs) -> cs |> Seq.length) >> Seq.head >> fst))
    |> Seq.toArray
    |> System.String.Concat

    

[<EntryPoint>]
let main argv =
    let file = argv |> Seq.head
    let signals = System.IO.File.ReadAllLines(file)
    signals |> decodeSignal |> printfn "Day 6 part 1 result: %s"
    0 // return an integer exit code
