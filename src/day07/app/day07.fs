module day07

open System
open System.Text.RegularExpressions

let getOrElse orelse = function Some x -> x
                              | None   -> orelse

let ipSegments ip =
    Regex.Matches(ip, @"\[(.*?)\]|.(.*?\b)") |> Seq.cast<Match> |> Seq.map (fun m -> m.Value)
    |> Seq.groupBy (fun s -> if s.StartsWith("[") then 1 else 0)
    |> Seq.sortBy fst
    |> Seq.toList
    |> function [ x; y]   -> (snd x, snd y)
              | [ (0,x) ] -> (x, Seq.empty)
              | [ (1,y) ] -> (Seq.empty, y)
              | _         -> (Seq.empty, Seq.empty)

let sslCodes (input : string) =
    input |> Seq.windowed 3
    |> Seq.choose (fun ([| x; y; z |] & ssl) ->
                    if x = z && x <> y then ssl |> String.Concat |> Some
                    else None)  

let getCode n f : string -> string seq = Seq.windowed n >> Seq.choose f

let getAbbas (input : string) =
    let filter = fun ([| x1; y1; y2; x2 |] & abba : char []) ->
                        if x1 = x2 &&  y1 = y2 && x1 <> y1 then abba |> String.Concat |> Some
                        else None
    input |> getCode 4 filter 

let getSsls (input : string) : string seq =
    let filter = fun ([| x; y; z |] & ssl : char []) ->
                    if x = z && x <> y then ssl |> String.Concat |> Some
                    else None
    input |> getCode 3 filter 

let tryIpTls ip =
    let notEmpty = not << Seq.isEmpty
    let (segs, hypers) = ip |> ipSegments
    if hypers |> Seq.collect getAbbas |> Seq.isEmpty && ip |> getAbbas |> notEmpty then Some ip
    else None

let tryIpSsl ip =
    let (segs, hypers) = ip |> ipSegments
    let hSsls = hypers |> Seq.collect getSsls |> Seq.map (Seq.take 2 >> Seq.rev >> Seq.toList) |> Set.ofSeq
    if segs |> Seq.collect getSsls |> Seq.map (Seq.take 2 >> Seq.toList) 
       |> Seq.exists (fun ss -> Set.contains ss hSsls)
       then Some ip
       else None 


[<EntryPoint>]
let main argv =
    let file = argv |> Array.head
    let ips = IO.File.ReadAllLines(file)
    ips |> Seq.choose tryIpTls
    |> Seq.length
    |> printfn "Day 7 part 1 result: %i"

    ips |> Seq.choose tryIpSsl
    |> Seq.length
    |> printfn "Day 7 part 2 result: %i"
    0 // return an integer exit code
