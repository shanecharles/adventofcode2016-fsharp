module day05

open System.Text
let md5 = System.Security.Cryptography.MD5.Create()

let calcHash prefix = string >> ASCIIEncoding.UTF8.GetBytes >> Seq.append prefix
                      >> Seq.toArray
                      >> md5.ComputeHash
let calcKeyCode1 n (prefix : string) = 
    let prefix' = prefix |> ASCIIEncoding.UTF8.GetBytes
    seq { for i in {n .. System.Int32.MaxValue} do
            let code = i |> calcHash prefix'
            if code.[0] = 0uy && code.[1] = 0uy && code.[2] < 16uy then
                yield code.[2] }

let calcKeyCode2 n (prefix : string) = 
    let prefix' = prefix |> ASCIIEncoding.UTF8.GetBytes
    seq {
        let hash = new System.Collections.Generic.HashSet<byte>() 
        for i in {n .. System.Int32.MaxValue} do
            let code = i |> calcHash prefix'
            if code.[0] = 0uy && code.[1] = 0uy && code.[2] < 8uy 
                && not <| hash.Contains(code.[2]) then
                    hash.Add(code.[2]) |> ignore
                    yield (code.[2], code.[3] |> sprintf "%02x" |> (fun s -> s.Substring(0,1))) }


let keyCodeToString : byte seq -> string = Seq.map (sprintf "%x") >> System.String.Concat

[<EntryPoint>]
let main argv =
    let input = argv |> Array.head
    input |> calcKeyCode1 0 |> Seq.take 8 
    |> keyCodeToString 
    |> printfn "Day 5 part 1 result: %s"

    input |> calcKeyCode2 0 |> Seq.take 8
    |> Seq.sort
    |> Seq.map snd
    |> System.String.Concat
    |> printfn "Day 5 part 2 result: %s"
    0 // return an integer exit code
