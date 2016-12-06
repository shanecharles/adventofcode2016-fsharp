module day04

let parseInput = 
    Seq.map (fun (s : string) -> s.Split('[')
                                 |> (fun [|data; chk|] ->
                                    let sector = data.Split('-') |> Seq.last |> int 
                                    (data.Replace("-" + (sector |> string), ""), sector, chk.Substring(0,5)))                                    )

let calcChecksum data = data |> Seq.filter (fun c -> c >= 'a')
                        |> Seq.groupBy id
                        |> Seq.map (fun (c,cs) -> cs |> Seq.length, c)
                        |> Seq.groupBy fst
                        |> Seq.sortByDescending (fun (count,_) -> count)
                        |> Seq.collect (snd >> Seq.map snd >> Seq.sort)
                        |> Seq.chunkBySize 5
                        |> Seq.head

let nextChar : byte -> byte = 
    (+) 1uy >> function c' when (byte 'z') < c' -> (byte 'a')
                      | c'                      -> c'

let decipher ((data, sector) : string * int) = 
    let rec inc (n : int) (b : byte) = if n = 0 then b
                                       else inc (n - 1) (nextChar b)  

    data |> Seq.map ((function '-' -> byte ' '
                             | x   -> x |> byte |> (inc sector)) >> System.Convert.ToChar)
    |> System.String.Concat, sector
    

let validateData (data, sectorId, checksum) = 
    if (checksum |> Seq.toArray) = (data |> calcChecksum) then Some (data, sectorId)
    else None

[<EntryPoint>]
let main argv =
    let file = argv |> Array.head
    let validRooms = System.IO.File.ReadAllLines(file) |> parseInput 
                     |> Seq.choose validateData

    validRooms |> Seq.map snd
    |> Seq.sum
    |> printfn "Day 4 part 1 result: %i" 

    validRooms |> Seq.map decipher
    |> Seq.filter (fun (name, _) -> name.StartsWith("northpole"))
    |> printfn "Day 4 part 2 result: %A" 
    
    0 // return an integer exit code
