#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

type Direction =
    | U
    | D
    | L
    | R
type Section = { direction : Direction; steps : int }
type Location = int * int
type Path = Location list

let parse (section : string) =
    let direction = section.Chars 0
    let stepsize = section.Substring 1 |> int
    let d =
        match direction with
        | 'U' -> U
        | 'D' -> D
        | 'L' -> L
        | 'R' -> R
        | err -> failwithf "Unknown direction: %c" err
    { direction = d; steps = stepsize }

let step (x,y) direction =
    match direction with
    | U -> (x, y + 1)
    | D -> (x, y - 1)
    | L -> (x - 1, y)
    | R -> (x + 1, y)

let rec move location section =
    if section.steps = 0 then ([], location)
    else 
        let s = step location section.direction
        let (rest, ending) = move s { section with steps = section.steps - 1 }
        ( s :: rest, ending)

type State = { location : Location; path : Path }
let path state section = 
    let (p, endLocation) = move state.location section
    { state with location = endLocation; path = (state.path |> List.append p) }

let pathFor sections =
    sections
    |> Seq.fold path { location = (0,0); path = [] }
    |> (fun s -> s.path)

let intersections paths =
    Set.intersectMany (paths |> Seq.map Set.ofList)
    |> Set.toList

let manhattan (x1,y1) (x2,y2) =
    abs (x1 - x2) + abs (y1 - y2)

printf "Testing..."

test <@ parse "R1005" = { direction = R; steps = 1005 } @>
test <@ pathFor [ {direction = U; steps = 1} ; {direction=R;steps= 2} ] = [(1, 1); (2, 1); (0, 1)] @>
test <@ intersections [[(1,1); (1,2); (1,3)]; [(0,1);(1,3);(1,2)]] = [(1,2);(1,3)] @>

printfn "..done!"

let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "\input.txt") |> Seq.toList
let parsedWires = input |> List.map (fun wire -> wire.Split([|','|]) |> Seq.toList |> List.map parse)

let candidates =
    parsedWires 
    |> List.map pathFor 
    |> intersections

candidates |> List.map (manhattan (0,0)) |> List.min

