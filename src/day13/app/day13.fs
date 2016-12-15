module day13

type Point = int * int

let isWall number (x,y) =
    x*x + 3*x + 2*x*y + y + y*y + number
    |> (fun n -> System.Convert.ToString(n,2))
    |> Seq.filter ((=)'1')
    |> Seq.length
    |> (fun n -> n % 2 = 1) 

let getSteps (x,y) = seq { for i in -1 .. 1 do 
                           for j in -1 .. 1 do
                           if (i <> 0 || j <> 0) && x+i >=0 && y+j >=0 then yield (x+i,y+j) }

let findPaths number dest =
    let isWall' = isWall number
    let rec step ((pos :: rest) as acc) =
        seq { if pos = dest then yield acc
              else 
                let next = getSteps pos 
                           |> Seq.filter (fun s -> s |> (not << isWall') 
                                                || rest |> Seq.forall (((=)s) >> not))
                for n in next do
                    printfn "%A" n
                    yield! step (n :: acc) 
        }
    step [(1,1)]

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code
