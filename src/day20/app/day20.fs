module day20

let parseLine (l : string) = l.Split('-') |> Array.map int64

let openIps blockedIps =
    let rec nextIp ip blocks = 
        seq { match blocks with 
              | []         -> ()
              | (s,e) :: t -> 
                if ip < s then yield! {ip .. s - 1L}     
                yield! nextIp (max ip (e + 1L)) t }
    nextIp 0L (blockedIps |> Seq.sortBy fst |> Seq.toList)

let parseInputFile = System.IO.File.ReadAllLines 
                     >> Array.map (parseLine >> function [|x;y|] -> (x,y)
                                                       | _       -> failwith "invalid input")

[<EntryPoint>]
let main argv =
    let input = argv.[0] |> parseInputFile
    input |> openIps |> Seq.head
    |> printfn "Day 20 part 1 result: %d"
    
    0 // return an integer exit code
