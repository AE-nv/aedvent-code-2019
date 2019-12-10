#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

type Space = Empty | Asteroid

let parseChar =
    function
    | '#' -> Asteroid
    | '.' -> Empty

let parseText lines =
    lines |> List.map (List.ofSeq >> List.map parseChar)

let toMap parsed = 
    parsed
    |> List.indexed
    |> List.collect (fun (ri,row) -> row |> List.indexed |> List.map (fun (ci,s) -> (ci,ri), s))

let example = [ ".#..#"
                "....."
                "#####"
                "....#"
                "...##"]
let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> List.ofSeq

let asteroids =             
    example
    |> parseText
    |> toMap
    |> List.filter (function | (_, Asteroid) -> true | _ -> false)
    |> List.map fst

let vector (x,y) (xo,yo) = xo-x,yo-y

let vectors asts a = 
    asts |> List.map (vector a)

let onLine (x1,y1) (x2,y2) (x3,y3) = 
    0 = x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)

let rec los loc vecs = 
    match vecs with
    | [] -> []
    | v :: vs -> 
        let rem = vs |> List.filter (fun x -> onLine x v (0,0) |> not)
        v :: (los loc rem)

let vecs =
    asteroids
    |> List.map (fun a -> a, vectors (asteroids |> List.except [a]) a)
    |> List.map (fun (a, v) -> (a, los a v |> List.length))
    |> List.map snd
    |> List.max