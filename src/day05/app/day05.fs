module day05

open System.Text
let md5 = System.Security.Cryptography.MD5.Create()

let calcHash (data : byte []) = md5.ComputeHash(data)

let calcKeyCode (prefix : string) = 
                         let prefix' = prefix |> ASCIIEncoding.UTF8.GetBytes
                         seq { for i in {0 .. System.Int32.MaxValue} do
                                let code = i |> string |> ASCIIEncoding.UTF8.GetBytes |> Seq.append prefix'
                                           |> Seq.toArray
                                           |> calcHash
                                if code.[0] = 0uy && code.[1] = 0uy && code.[2] < 16uy then
                                    yield code.[2] }


[<EntryPoint>]
let main argv =
    let input = argv |> Array.head
    input |> calcKeyCode |> Seq.take 8 |> Seq.toArray |> ASCIIEncoding.UTF8.GetString
    |> printfn "Day 5 part 1 result: %s"
    0 // return an integer exit code
