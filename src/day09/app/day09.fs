module day09

open System
let parseMarker (marker : string) = 
    let removeBrackets = (function '(' | ')' -> false
                                 | _         -> true)
    marker.Split('x')|> Seq.map ( seq >> Seq.filter removeBrackets
                                  >> Seq.toArray >> System.String.Concat >> int64)
    |> Seq.toList
    |> function chars :: reps :: _ -> (chars, reps)
              | _                  -> (0L,0L)

type Read = 
    | Regular of char list
    | OpenMarker of char list
    | Marker of int64 * int64 * int64 * char list

let handleRegular cs = function '(' -> [OpenMarker []; Regular (cs |> List.rev)]
                              | c   -> [Regular (c :: cs)]

let handleOpenMarker (cs : char list) = function
                          | ')' -> let (chars, reps) = cs |> List.rev |> String.Concat |> parseMarker
                                   [Marker (chars, reps, chars, [])]
                          | c   -> [OpenMarker (c :: cs)]

let handleMarker (tot, rep, cn, cs) c =
    if cn = 1L then [Regular []; Marker (tot, rep, 0L, (c :: cs) |> List.rev)]
    else [Marker (tot,rep, cn-1L, c :: cs)]                                        
let folder (h :: t) c =
    let nh = match h with
             | Regular cs           -> handleRegular cs c
             | OpenMarker cs        -> handleOpenMarker cs c
             | Marker (tot, rep, cn, cs) -> handleMarker (tot, rep, cn, cs) c
    nh @ t

let parseInput data = 
    data |> Seq.fold folder [Regular []]

let length mCounter segments = 
    segments |> Seq.map (function Regular cs           -> int64 (cs |> Seq.length)
                                | Marker (c, m, _, cs) -> mCounter c m cs
                                | _                    -> 0L)
    |> Seq.sum

let decompressV2Marker c (m : int64) cs = 
    let rec decompressMarkers mult = 
        function Regular cs            -> mult * (int64 (cs |> Seq.length))
                | Marker (c, m, _, cs) -> cs |> parseInput |> Seq.map (decompressMarkers (m * mult))
                                          |> Seq.sum |> int64
                | _                    -> 0L
    decompressMarkers 1L (Marker (c, m, 0L, cs))

let lengthV1 = length (fun _ m cs -> m * (int64)(cs |> Seq.length))
let lengthV2 : Read list -> int64 = length decompressV2Marker

[<EntryPoint>]
let main argv =
    let file = argv |> Seq.head
    let input = IO.File.ReadLines(file) |> Seq.head |> parseInput
    input |> lengthV1
    |> printfn "Day 9 part 1 result: %d"

    input |> lengthV2
    |> printfn "Day 9 part 2 result: %d"
    0 // return an integer exit code