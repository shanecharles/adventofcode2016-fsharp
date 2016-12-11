module day10

type BotData = { Id : string; Values : int list; Low : string; Hi : string }
type OutputData = { Id : string; Value : int }

type MicroChipContainer = 
    | Bot of BotData
    | Output of OutputData

let setupBotNetwork (lines : string []) =
    let cache = new System.Collections.Generic.Dictionary<string, MicroChipContainer>()
    let addValue key v = 
        if cache.ContainsKey(key) then 
            cache.[key] <- match cache.[key] with
                           | Bot d    -> Bot { d with Values = (v :: d.Values) |> List.sort }
                           | Output d -> Output { d with Value = v}
        else
            cache.[key] <- match key |> Seq.head with 
                           | 'b' -> Bot { Id = key; Values =[v]; Low = ""; Hi = "" }
                           | _   -> Output { Id = key; Value = v }
    
    let updateBotProps key low hi = 
        if cache.ContainsKey(key) then 
            cache.[key] <- cache.[key] |> function Bot d    -> Bot {d with Low = low; Hi = hi}
                                                 | Output d -> Output d 
        else
            cache.[key] <- Bot {Id = key; Values = []; Low = low; Hi = hi} 
        
    lines |> Seq.iter (fun l -> 
        match l.Split(' ') with
        | [| "bot"; ident; _; _; _; lowC; lId; _; _; _; hiC; hId |] -> 
                updateBotProps ("b"+ident) (lowC.Substring(0,1) + lId) (hiC.Substring(0,1) + hId)
        | [| "value"; x; _; _; mc; ident|] -> 
                addValue (mc.Substring(0,1) + ident) (int x)
        | _                                -> ())

    let rec propagate key value =
        addValue key value
        match cache.[key] with
        | Bot {Values = [l;h]; Low = low; Hi = hi} -> propagate low l; propagate hi h;
        | _                                        -> () 
          
    cache |> Seq.choose (fun kv -> match kv.Value with 
                                   | Bot d -> if 2 = (d.Values |> Seq.length) then Some kv.Value else None 
                                   | _     -> None)
    |> Seq.toList
    |> List.iter (function Bot { Low = low; Hi = hi; Values = [l;h] } -> propagate low l; propagate hi h)
    
    cache |> Seq.map (fun kv -> kv.Value)


[<EntryPoint>]
let main argv =
    let inputs = argv |> Seq.head |> System.IO.File.ReadAllLines
    let network = setupBotNetwork inputs |> Seq.toList
    network |> Seq.find (fun n -> match n with
                                  | Bot {Values = vs} -> vs = [17;61]
                                  | _                 -> false)
    |> (fun (Bot {Id = bid}) -> printfn "Day 10 part 1 result: %s" (bid |> Seq.skip 1 |> System.String.Concat))
    network |> Seq.choose (fun n -> match n with 
                                    | Output {Id = oid; Value = v} when oid = "o0" || oid = "o1" || oid = "o2" -> Some v
                                    | _                                                                     -> None)
    |> Seq.reduce (*)
    |> printfn "Day 10 part 2 result: %d"
    0 // return an integer exit code
