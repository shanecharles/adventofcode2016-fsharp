module day16

let mapChar = function '1' -> 1uy
                     | '0' -> 0uy
let flipBit x = (x + 1uy) % 2uy 

let flipBitsAndReverse : byte seq -> byte seq = Seq.map flipBit >> Seq.rev

let curveData (size, data) =
    (size * 2 + 1), seq { yield! data 
                          yield 0uy 
                          yield! flipBitsAndReverse data}

let pairWise : byte seq -> byte = Seq.sum >> function 1uy -> 0uy
                                                    | _   -> 1uy

let expandData limit data = 
    let rec fill = function size, ds when limit <= size -> ds |> Seq.take limit
                          | data'                       -> data' |> curveData |> fill
    fill (data |> Seq.length, data)

let computeChecksum data =
    let computeChecksum' = Seq.chunkBySize 2 >> Seq.map pairWise
    let rec checksum d = 
        if d |> Seq.length |> (fun l -> l % 2 = 1)
        then d
        else d |> computeChecksum' |> checksum
    data |> computeChecksum' |> checksum
    
[<EntryPoint>]
let main argv =
    let initial = argv.[0] |> Seq.map mapChar
    initial |> expandData 272 |> computeChecksum |> Seq.toArray 
    |> System.String.Concat
    |> printfn "Day 16 part 1 result: %s"

    0 // return an integer exit code
