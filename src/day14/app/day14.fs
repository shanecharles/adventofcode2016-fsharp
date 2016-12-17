module day14

let consecutive cs = cs |> Seq.fold (fun state c -> match state with
                                                    | (c',n) :: rest when c' = c -> (c', n+1) :: rest
                                                    | t -> (c, 1) :: t) []
                     |> Seq.rev

let getCandidateChars cs = cs |> Seq.filter (snd >> (<)2) |> Seq.toList
                           |> function 
                           | ((c,_) :: t) as l -> Some (c, l |> List.filter (snd >> (<)4) |> List.map fst)
                           | []                -> None  

let byteMap = {0uy .. 255uy} |> Seq.map (fun i -> (i, i |> sprintf "%02x" |> Seq.map byte |> Seq.toArray)) |> Map.ofSeq

let basicHasher salt i = 
    use md5 = System.Security.Cryptography.MD5.Create ()
    salt + (string i) |> Seq.map byte |> Seq.toArray |> md5.ComputeHash

let stretchHasher (salt : string) (i : int) = 
    use md5 = System.Security.Cryptography.MD5.Create ()
    let recycler : byte [] -> byte [] = Array.collect (fun b -> byteMap.[b]) >> md5.ComputeHash
    salt + (string i) |> Seq.map byte |> Seq.toArray |> md5.ComputeHash
    |> function hs -> [|1 .. 2016|] |> Array.fold (fun (state : byte []) _ -> recycler state) hs

let computeKeys batchSize hasher salt =
    let getHashConsecutive i = hasher salt i
                                |> Seq.collect (fun b -> byteMap.[b] |> Seq.map char)
                                |> consecutive 
                                |> getCandidateChars
                                |> Option.bind (fun (c, n) -> Some (i, c, n))

    let nextBatch st en = [|st .. en|] |> Array.Parallel.choose getHashConsecutive

    let rec nextKey (cache : (int * char * char list) []) (index : int) =
        seq {
            let nIndex = index + batchSize
            for (i, c, _) in cache  do 
                if i + 1000 > index 
                then yield! nextKey (nextBatch (index+1)  nIndex |> Array.append (cache |> Array.skipWhile (fun (i',_,_) -> i' < i))) nIndex
                else 
                    if cache |> Seq.exists (fun (i', _, cs) -> i < i' && i' <= i + 1000 && cs |> Seq.exists (fun c' -> c' = c))
                    then yield i
        }
    nextKey (nextBatch 0 batchSize) batchSize

[<EntryPoint>]
let main argv =
    let salt = argv.[0]

    salt |> computeKeys 2000 basicHasher |> Seq.skip 63 |> Seq.head
    |> printfn "Day 14 part 1 result: %d"

    salt |> computeKeys 2000 stretchHasher |> Seq.skip 63 |> Seq.head
    |> printfn "Day 14 part 2 result: %d"

    0 // return an integer exit code
