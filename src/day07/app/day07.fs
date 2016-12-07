module day07

open System
open System.Text.RegularExpressions
let getHyperNetSequences ip =
    let r = Regex("\[(.*?)\]")
    let matches = r.Matches(ip)
    matches |> Seq.cast<Match> |> Seq.map (fun m -> m.Value)

let tryGetAbbas (input : string) =
    input |> Seq.windowed 4
    |> Seq.choose (fun ([| x1; y1; y2; x2 |] & abba) ->
                if x1 = x2 &&  y1 = y2 && x1 <> y1 then abba |> String.Concat |> Some
                else None)

let tryIp7Tls ip =
    let notEmpty = not << Seq.isEmpty
    let isHyperNet = ip |> getHyperNetSequences |> Seq.collect tryGetAbbas |> notEmpty
    if not <| isHyperNet && ip |> tryGetAbbas |> notEmpty then Some ip
    else None


[<EntryPoint>]
let main argv =
    let file = argv |> Array.head
    let ips = IO.File.ReadAllLines(file)
    ips |> Seq.choose tryIp7Tls
    |> Seq.length
    |> printfn "Day 7 part 1 result: %i"
    0 // return an integer exit code
