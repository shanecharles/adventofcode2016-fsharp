module day13

let isPath number (x,y) =
    x*x + 3*x + 2*x*y + y + y*y + number
    |> (fun n -> System.Convert.ToString(n,2))
    |> Seq.filter ((=)'1')
    |> Seq.length
    |> (fun n -> n % 2 = 0) 

let getSteps (x,y) = [|(x-1,y); (x+1,y); (x,y-1); (x,y+1)|] |> Array.filter (fun (x',y') -> x' >= 0 && y' >= 0)
let advance pathCheck steps = 
    match steps with
    | []          -> [||]
    | [h]         -> h |> getSteps |> Array.filter pathCheck
    | h :: t -> h |> getSteps |> Array.filter (fun x -> pathCheck x && t |> Seq.exists ((=)x) |> not)
    |> function [||] -> None
              | ns   -> Some ns
    |> Option.bind (fun ns -> Some [ for n in ns -> (n :: steps) ])

let shortestPath number dest =
    let openPath = isPath number
    let rec path steps =
        match steps |> List.tryFind (fun (h :: _) -> h = dest) with
        | Some ss -> ss
        | None    ->
            steps |> List.choose (advance openPath) |> List.concat |> path

    path [[(1,1)]]

let pointsVisited number iterations = 
    let openPath = isPath number
    let cache = new System.Collections.Generic.HashSet<int*int>()
    let addToCache ns = ns |> Array.iter (cache.Add >> ignore); ns
    let getNextSteps = Seq.collect (getSteps >> Array.filter openPath)
                       >> Seq.filter (cache.Contains >> not)
                       >> Seq.toArray
    cache.Add (1,1) |> ignore
    let rec loop count positions =
        if count <= 0 then cache |> Seq.toArray
        else positions |> getNextSteps
             |> addToCache
             |> loop (count-1)
    loop 50 [|(1,1)|]

[<EntryPoint>]
let main argv =
    let number = argv.[0] |> int
    shortestPath number (31,39) |> Seq.length |> ((+) -1)
    |> printfn "Day 13 part 1 result: %d"

    pointsVisited number 50 |> Array.length
    |> printfn "Day 13 part 2 result: %d"
    0 // return an integer exit code
