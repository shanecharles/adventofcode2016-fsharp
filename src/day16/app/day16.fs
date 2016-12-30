module day16

let mapChar = function '1' -> 1uy
                     | '0' -> 0uy

let mapInputString : string -> byte [] = Seq.map mapChar >> Seq.toArray
let flipBit x = (x + 1uy) % 2uy 

let flipBitsAndReverse : byte [] -> byte [] = Array.map flipBit >> Array.rev

let curveData data = [| yield! data 
                        yield 0uy 
                        yield! flipBitsAndReverse data |]

let pairChecksum : byte [] -> byte = Array.sum >> function 1uy -> 0uy
                                                         | _   -> 1uy

let expandData limit (data : byte []) = 
    let rec fill (ds : byte []) = 
            if ds.Length >= limit then ds |> Array.take limit
            else ds |> curveData |> fill
    fill data

let computeChecksum data =
    let computeChecksum' = Array.chunkBySize 2 >> Array.map pairChecksum
    let rec checksum (d : byte []) = 
        if d.Length % 2 = 1
        then d
        else d |> computeChecksum' |> checksum
    data |> computeChecksum' |> checksum

let fillAndChecksum size = 
    expandData size >> computeChecksum >> Seq.toArray >> System.String.Concat
    
[<EntryPoint>]
let main argv =
    let initial = argv.[0] |> Seq.map mapChar |> Seq.toArray
    initial |> fillAndChecksum 272
    |> printfn "Day 16 part 1 result: %s"

    initial |> fillAndChecksum 35651584
    |> printfn "Day 16 part 2 result: %s"

    0 // return an integer exit code
