#load "IntCode.fsx"
open IntCode

//IntCode.t ()

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
let p : IntCode.Program = input.Split([|','|]) |> Seq.map int64 |> Seq.toList
let initProg = (IntCode.initState [] p)

let loaded = initProg |> IntCode.run

let split maze =
    let rec split maze =
        match maze with
        | [] -> []
        | '\010' :: ms -> [] :: (split ms)
        | c :: ms ->
            let split_h :: split_ms = split ms
            (c :: split_h) :: split_ms
    split maze |> List.rev |> List.skip 1 |> List.rev

let toString (maze : char list list) = 
    [
        for row in maze do
            for cell in row do
                yield cell |> string
            yield "\n"
    ] |> String.concat ""

let toMap (maze : char list list) =
    [
        for (y,row) in maze |> List.indexed do
            for (x,cell) in (row |> List.indexed) do
                yield (x,y),cell
    ]
    |> Map.ofList

let neighbours map (x,y) =
    let deltas = [(-1,0);(1,0);(0,-1);(0,1)]
    deltas |> List.map (fun (dx,dy) -> (x+dx,y+dy))  |> List.choose (fun d -> map |> Map.tryFind d)

let rawMaze = loaded.output |> List.rev |> List.map (int >> char)
let maze = rawMaze |> split 
toString maze
let indexed = toMap maze
let scaffolds = indexed |> Map.toList |> List.filter (fun (_,c) -> c = '#') |> List.map (fst >> (fun x -> x,x)) |> Map.ofList
let bot = indexed |> Map.toList |> List.find (fun (_,b) -> b = '^') |> fst

type Orientation = Up | Down | Left | Right
type Location = int * int
type Command = 
    | Turn of Orientation
    | Forward
type State = { location : Location; orientation: Orientation; commands : Command list }


let nextLocation (x,y) =
    function
    | Up -> (x, y-1)
    | Down -> (x, y+1)
    | Left -> (x-1, y)
    | Right -> (x+1, y)
let turns =
    function
    | Up -> [Left;Right]
    | Down -> [Left;Right]
    | Left -> [Up;Down]
    | Right -> [Up;Down]


let start = { location = bot; orientation = Up; commands = []}
let rec go scaffolds state : State =
    printfn "%A" state
    let n = nextLocation state.location state.orientation
    let fwd = scaffolds |> Map.tryFind n
    match fwd with
    | Some _ -> 
        { state with location = n; commands = Forward :: state.commands }
    | None ->
        let next_turns = 
            state.orientation 
            |> turns 
            |> List.filter (fun t -> scaffolds |> Map.containsKey (nextLocation state.location t))
        match next_turns with
        | [] -> state
        | [turn] -> { state with orientation = turn; commands = (Turn turn) :: state.commands }
        | err -> failwithf "whoops %A" err
    |> go scaffolds

go scaffolds start