module day02
let input = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "/input.txt")
let validPosition x = 0 <= x && x <= 2
let getOrElse defVal = function Some x -> x
                               | None  -> defVal

type KeyPadPosition = int * int
let keypad1 = Map.ofList [ ((0,2), '1')
                           ((1,2), '2')
                           ((2,2), '3')
                           ((0,1), '4')
                           ((1,1), '5')
                           ((2,1), '6')
                           ((0,0), '7')
                           ((1,0), '8')
                           ((2,0), '9') ]

let keypad2 = Map.ofList [                             ((2,4), '1');
                                         ((1,3), '2'); ((2,3), '3'); ((3,3), '4');
                           ((0,2), '5'); ((1,2), '6'); ((2,2), '7'); ((3,2), '8'); ((4,2), '9');
                                         ((1,1), 'A'); ((2,1), 'B'); ((3,1), 'C');
                                                       ((2,0), 'D')]
let validKeyPadKey kp (pos : int * int) = 
    kp |> Map.tryFind pos 
    |> Option.bind (fun _ -> Some pos)

let nextKey (x, y) = if validPosition x && validPosition y then Some (x,y)
                     else None

let moveKeys f (x,y) = function 
                       | 'U' -> f (x, (y + 1))
                       | 'D' -> f (x, (y - 1))
                       | 'R' -> f ((x + 1), y)
                       | 'L' -> f ((x - 1), y)
                       >> getOrElse (x,y)

let scanKeyLine f keyPos moves =
    moves |> Seq.fold (moveKeys f) keyPos

let getKey kp pos = kp |> Map.find pos
let scanLines keypad keyPos moveList : char list =
    moveList |> Seq.scan (fun state moves -> moves |> scanKeyLine (validKeyPadKey keypad) state) keyPos
    |> Seq.skip 1
    |> Seq.map (getKey keypad)
    |> Seq.toList

let scanKeypad1 : string seq -> char list = scanLines keypad1 (1,1)
let scanKeypad2 : string seq -> char list  = scanLines keypad2 (0,2)

[<EntryPoint>]
let main argv =
    let file = argv |> Array.head 
    let moveLines = System.IO.File.ReadAllLines(file)
    moveLines |> scanKeypad1 |> printfn "Day 2 solution 1: %A"
    moveLines |> scanKeypad2 |> printfn "Day 2 solution 2: %A" 
    0 // return an integer exit code
