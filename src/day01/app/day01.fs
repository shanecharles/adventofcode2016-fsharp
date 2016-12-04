module day01

let mapInputString (s : string) = 
    s.Split(',') |> Seq.map (fun s -> let d = s.Trim() 
                                      d |> Seq.head, d.Substring(1,d.Length - 1) |> int)

type Direction = North
               | East
               | South
               | West
let turnLeft = function North -> West
                      | East  -> North
                      | South -> East
                      | West  -> South
let turnRight = function North -> East
                        | East  -> South
                        | South -> West
                        | West  -> North        
let turnDirection direction = function
                              | 'L' -> turnLeft direction
                              | 'R' -> turnRight direction
                              | _   -> direction

type Point = { X: int; Y: int }

type Movement = Direction * int

let origin = {X = 0; Y = 0}

let applyX p d = {p with X = p.X + d}
let applyY p d = {p with Y = p.Y + d}

let turnAndMove (direction, _) (turn, move) =
    (turnDirection direction turn, move)
let mover point = function
                  | North, y -> applyY point y
                  | South, y -> applyY point -y
                  | East, x  -> applyX point x
                  | West, x  -> applyX point -x

let blockDistance point = System.Math.Abs(point.X) + System.Math.Abs(point.Y)
let getMovements : (char * int) seq -> Movement seq = Seq.scan turnAndMove (North,0)

let day1Solution1 : (char * int) seq -> int = 
    getMovements >> Seq.fold mover origin 
    >> blockDistance 

let pointsBetween point (direction, distance) =
    let apply,stepDirection = 
        match direction with
        | North -> applyY, 1 
        | East  -> applyX, 1
        | South -> applyY, -1
        | West  -> applyX, -1
    apply point (stepDirection * distance),
        seq { for i in 1 .. distance do 
                yield apply point (i * stepDirection)}

let duplicates (ps : Point seq) = 
    seq { let hash = new System.Collections.Generic.HashSet<Point>()
          for p in ps do 
            if hash.Contains(p) then yield p
            else hash.Add(p) |> ignore }

let firstDuplicateBlock = 
    Seq.fold (fun ((p,ps) :: _ as acc) movement -> 
                            (pointsBetween p movement) :: acc) [(origin, Seq.ofList [origin])]
    >> Seq.map snd
    >> Seq.rev
    >> Seq.concat
    >> duplicates
    >> Seq.head

let day1Solution2 : (char * int) seq -> int = 
    getMovements >> firstDuplicateBlock 
    >> blockDistance   

[<EntryPoint>]
let main argv =
    let file = Array.head argv
    let inputs = System.IO.File.ReadAllText(file)
                 |> mapInputString
    
    inputs |> day1Solution1 |> printfn "Day 1 part 1 result: %d" 
    inputs |> day1Solution2 |> printfn "Day 1 part 2 result: %d" 

    0 // return an integer exit code
