module day08

type Operation =
    | Rect of int * int
    | Row of int * int
    | Column of int * int

type Display (width,height) =
    let display = Array2D.init width height (fun x y -> false)

    let applyShift n f = Seq.concat >> Seq.take n >> Seq.toList >> List.iteri f
    member this.Display = display
    member this.TurnOnSection (x, y) =
        for x' in {0 .. (x - 1)} do
        for y' in {0 .. (y - 1)} do
            display.[x',y'] <- true

    member this.RotateColumn (x, shift) =
        let origin = height - shift
        seq { yield display.[x, origin .. (height - 1)] 
              yield display.[x, 0 .. origin] } 
        |> applyShift height (fun y v -> display.[x,y] <- v)
    
    member this.RotateRow (y,shift) =
        let origin = width - shift
        seq { yield display.[origin .. (width - 1), y]
              yield display.[0 .. origin,y] } 
        |> applyShift width (fun x v -> display.[x,y] <- v)

    member this.LitPixelCount () =
        display |> Seq.cast<bool> |> Seq.filter id |> Seq.length

let performOperation (disp : Display) = function
    | Rect (x,y)   -> disp.TurnOnSection (x,y)
    | Row  (y,n)   -> disp.RotateRow (y,n)
    | Column (x,n) -> disp.RotateColumn (x,n)

let parseOperation (line : string) =
    let parseNxM (v : string) = v.Split('x') |> function [|x; y|] -> int x, int y
    let parseN (v : string)   = v.Split('=') |> function [|_; n|] -> int n 
    line.Split(' ') |> function
    | [| "rect"; area |]         -> Rect (parseNxM area)
    | [| _; "row"; y; _; n |]    -> Row ((parseN y), int n)
    | [| _; "column"; x; _; n |] -> Column ((parseN x), int n)

[<EntryPoint>]
let main argv =
    let file = argv |> Seq.head
    let ops = System.IO.File.ReadAllLines file |> Seq.map parseOperation
              |> Seq.toList
    let d1 = Display(50, 6)
    let performOps = performOperation d1
    ops |> List.iter performOps
    d1.LitPixelCount () |> printfn "Day 8 part 1 result: %i" 
    0 // return an integer exit code
