#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote  

let parse lines = 
    seq {
        for y,row in (lines |> Seq.indexed) do
        for x,cell in (row |> Seq.indexed) do
        if cell = '#' then yield (x,y)
    }
    |> List.ofSeq

type Slope = Infinity | Slope of float

let slope (x1,y1) (x2,y2) : int * int * Slope =
    let (dx,dy) = (x2 - x1, y2 - y1)
    (sign dx, sign dy, if dx = 0 then Infinity else Slope ((float dy)/(float dx)))

let solve input =
    let asteroids = parse input
    
    asteroids
    |> List.map (fun a -> a, asteroids |> List.except [a] |> List.groupBy (slope a))
    |> List.map (fun (a,dirs) -> a,dirs |> Seq.length)
    |> List.map snd
    |> List.max

let t () =
    printf "Testing..."

    test <@ 
            let example = 
                [ ".#..#"
                  "....."
                  "#####"
                  "....#"
                  "...##"]
            solve example = 8 @>

    printfn "..done!"
t ()

let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> List.ofSeq
input |> solve