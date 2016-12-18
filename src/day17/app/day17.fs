module day17

type Point = int * int
let byteMap = {0uy .. 255uy} |> Seq.map (fun b -> b, (b |> sprintf "%02x" |> Seq.map (byte) |> Seq.toArray)) |> Map.ofSeq
let isDoorOpen b = 98uy <= b && b <= 102uy
let md5 = System.Security.Cryptography.MD5.Create()
let expandHash = Array.collect (fun b -> byteMap.[b])
let doors = [|'U'; 'D'; 'L'; 'R'|]
let gridLimit (x,y) = let valid z = 0 <= z && z <= 3
                      if valid x && valid y then Some (x,y)
                      else None
let getOrElse v = function Some x -> x
                         | None   -> v

let movePoint (x,y) = function 'U' -> x, y-1
                             | 'D' -> x, y+1
                             | 'L' -> x-1, y
                             | 'R' -> x+1, y
                      >> gridLimit
                      >> getOrElse (x,y)

let nextPossibleMoves (bytes : byte []) = 
    let md5 = System.Security.Cryptography.MD5.Create()
    bytes |> md5.ComputeHash |> expandHash |> Seq.map isDoorOpen |> Seq.zip doors |> Seq.filter snd |> Seq.map fst

let advance (pos, ss) =
    seq {
        match nextPossibleMoves ss |> Seq.toArray with
        | [||] -> ()
        | xs   -> yield! xs |> Seq.map (fun x -> (movePoint pos x), (Seq.append ss ([x] |> Seq.map byte) |> Seq.toArray))
    }

let shortestPath (pos, ss) =
    let rec path ppss =    
        let next = ppss |> Seq.collect advance |> Seq.toArray
        match next, next |> Array.tryFind (fst >> ((=)(3,3))) with
        | [||], _       -> [||]
        | _, Some (p,s) -> s
        | _             -> path next
    path [|(pos, ss)|]

let stringToBytes : char seq -> byte [] = Seq.map byte >> Seq.toArray 
let bytesToString = Array.map char >> System.String.Concat

[<EntryPoint>]
let main argv =
    let input = ((0,0), argv.[0] |> stringToBytes)
    input |> shortestPath |> Array.skip (argv.[0].Length)
    |> bytesToString
    |> printfn "Day 17 part 1 result: %s"
    0 // return an integer exit code
