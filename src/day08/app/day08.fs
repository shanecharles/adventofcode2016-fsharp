module day08

type Display (width,height) =
    let display = Array2D.init width height (fun x y -> false)

    let applyShift f = Seq.concat >> Seq.toList >> List.iteri f
    member this.Display = display
    member this.TurnOnSection (x, y) =
        for x' in {0 .. (x - 1)} do
        for y' in {0 .. (y - 1)} do
            display.[x',y'] <- true

    member this.RotateColumn (x, shift) =
        let origin = height - shift
        seq { yield display.[x, origin ..] 
              yield display.[x, .. origin - 1] } 
        |> applyShift (fun y v -> display.[x,y] <- v)
    
    member this.RotateRow (y,shift) =
        let origin = width - shift
        seq { yield display.[origin .., y]
              yield display.[.. origin - 1, y] } 
        |> applyShift (fun x v -> display.[x,y] <- v)

    member this.LitPixelCount () =
        display |> Seq.cast<bool> |> Seq.filter id |> Seq.length

    member this.Print () =
        let printable = function true -> '0'
                               | _    -> ' '
        {0 .. height-1} |> Seq.iteri 
            (fun i y -> display.[0.., y] |> Seq.chunkBySize 5
                        |> Seq.toList
                        |> List.iter (Array.iter (printable >> printf "%c") >> (fun _ -> printf " "))
                        printfn "")

type Operation =
    | Rect of int * int
    | Row of int * int
    | Column of int * int
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
    printfn "Day 8 part 2 result: "
    d1.Print ()
    0 // return an integer exit code