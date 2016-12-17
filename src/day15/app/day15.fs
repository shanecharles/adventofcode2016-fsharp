module day15

type Disc = { Positions : int; Start : int }
let advanceTime disc time = (time + disc.Start) % disc.Positions 

let timeSeq discs =
    seq {
        for i in 0 .. System.Int32.MaxValue do
            if discs |> Seq.mapi (fun i' d -> advanceTime d (i'+1+i)) |> Seq.forall (fun x -> x = 0)
            then yield i
    }

[<EntryPoint>]
let main argv =
    let discs = argv.[0] |> System.IO.File.ReadAllLines
                |> Seq.map (fun l -> let ls = l.Split(' ') in {Positions=(int ls.[3]); Start=(int (ls.[11].Replace(".","")))})
                |> Seq.toList

    discs |> timeSeq |> Seq.head
    |> printfn "Day 15 part 1 result: %d"

    [{Positions=11; Start=0}] |> List.append discs |> timeSeq |> Seq.head
    |> printfn "Day 15 part 2 result: %d"
    0 // return an integer exit code
