module day09

open System
let parseMarker (marker : string) = 
    let removeBrackets = (function '(' | ')' -> false
                                 | _         -> true)
    marker.Split('x')|> Seq.map ( seq >> Seq.filter removeBrackets
                                  >> Seq.toArray >> System.String.Concat >> int)
    |> Seq.toList
    |> function chars :: reps :: _ -> (chars, reps)
              | _                  -> (0,0)

type Segments =
    | Compressed of int * char seq
    | Uncompressed of char seq

type Read = 
    | Regular of char list
    | OpenMarker of char list
    | Marker of int * int * int * char list

let handleRegular cs = function '(' -> [OpenMarker []; Regular (cs |> List.rev)]
                              | c   -> [Regular (c :: cs)]

let handleOpenMarker (cs : char list) = function
                          | ')' -> let (chars, reps) = cs |> List.rev |> String.Concat |> parseMarker
                                   [Marker (chars, reps, chars, [])]
                          | c   -> [OpenMarker (c :: cs)]

let handleMarker (tot, rep, cn, cs) c =
    if cn = 1 then [Regular []; Marker (tot, rep, 0, (c :: cs) |> List.rev)]
    else [Marker (tot,rep, cn-1, c :: cs)]                                        
let folder (h :: t) c =
    let nh = match h with
             | Regular cs           -> handleRegular cs c
             | OpenMarker cs        -> handleOpenMarker cs c
             | Marker (tot, rep, cn, cs) -> handleMarker (tot, rep, cn, cs) c
    nh @ t

let parseInput data = 
    data |> Seq.fold folder [Regular []]

let lengthV1 segments = 
    segments |> Seq.map (function Regular cs           -> cs |> Seq.length
                                | Marker (_, m, _, cs) -> m * (cs |> Seq.length) 
                                | _                    -> 0)
    |> Seq.sum

[<EntryPoint>]
let main argv =
    let file = argv |> Seq.head
    let input = IO.File.ReadAllText(file).Replace("\n","").Replace(" ","") |> parseInput
    input |> lengthV1
    |> printfn "Day 9 part 1 result: %i"
    0 // return an integer exit code
