#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
#r @"..\..\dependencies\jo\paket\packages\FsAlg\lib\FSAlg.dll"
open Swensen.Unquote  
open FsAlg.Generic

let parse lines = 
    seq {
        for y,row in (lines |> Seq.indexed) do
        for x,cell in (row |> Seq.indexed) do
        if cell = '#' then yield (x,y)
    }
    |> List.ofSeq

let angle a b = 
    let (x,y),(xb,yb) = a,b
    let dx,dy = (x - xb), (y - yb)
    -1

let angle (x1,y1) (x2,y2) =
    let v1 = vector [float x1; float y1]
    let v2 = vector [float x2; float y2]
    let dv = v2 - v1
    let dhorizontal = vector [float System.Int64.MaxValue; float y1]
    let dot = dhorizontal * dv
    dot / dhorizontal.

let solve input =
    let asteroids = parse input
    
    asteroids
    |> List.map (fun a -> a, asteroids |> List.except [a] |> List.groupBy (angle a))
    |> List.map (fun (a,dirs) -> a,dirs |> Seq.length)
    |> List.map snd
    |> List.max

let t () =
    printf "Testing..."
    test <@ norm (3,4) = 5.0 @>
    test <@ normalize (3,4) = (0.6,0.8) @>
    test <@ dot (1,2) (3,4) = 3 @>

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

//let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> List.ofSeq
//input |> solve //WRONG: 265