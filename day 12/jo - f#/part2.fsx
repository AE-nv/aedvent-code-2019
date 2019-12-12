type Vector = {x : int; y : int; z : int }
type Moon = { id : int; position : Vector; velocity : Vector }

let init id x y z = { id = id; position = { x = x; y = y; z = z}; velocity = { x =0; y = 0; z = 0 }}

let rec pairs = 
    function
    | x :: xs -> 
        let ofX = xs |> List.map (fun y -> (x,y))
        let r = pairs xs
        List.append ofX r
    | [] -> []

let gravity one other =
    let step a b =
        if a = b then 0
        elif a < b then 1
        else -1
    { x = step one.x other.x
      y = step one.y other.y
      z = step one.z other.z }

let applyGravity (id, v) moons = 
    let moon = moons |> Map.find id
    let updated =
        { moon with velocity = 
            { 
                x = moon.velocity.x + v.x 
                y = moon.velocity.y + v.y 
                z = moon.velocity.z + v.z 
            }
        }
    moons |> Map.add updated.id updated

let applyVelocity moon = 
    let v = moon.velocity
    { moon with position = { 
            x = moon.position.x + v.x 
            y = moon.position.y + v.y
            z = moon.position.z + v.z         
        } 
    }

let step moons =
    let gravityVecs =
        moons
        |> (Map.toList >> List.map snd)
        |> pairs
        |> List.collect (fun (a,b) -> [a.id, gravity a.position b.position; b.id, gravity b.position a.position])

    let updatedGravity = gravityVecs |> List.fold (fun ms v -> applyGravity v ms) moons

    updatedGravity |> Map.map (fun _ v -> applyVelocity v)

let rec takeSteps s moons =
    if s = 0 then moons
    else takeSteps (s - 1) (step moons)

let example = [
    init 0 -1 0 2
    init 1 2 -10 -7
    init 2 4 -8 8
    init 3 3 5 -1 ]

let input = [ init 0 -9 10 -1;init 1 -14 -8 14;init 2 1 5 6;init 3  -19 7 8 ]

let periodOf moons =
    let rec run moons todos gen acc =
        match todos with
        | [] -> acc
        | _ ->
            if gen % 1_000_000L = 0L then printfn "%A" gen
            let next = step moons
            //TODO: redesign so we only simulate once instead of or every period
            todos |> Map.map
            let nm = next |> Map.find id
            if moon = nm then 
                printfn "FOUND A PERIOD: %d - %A" id gen
                gen
            else run next (gen + 1L)
    run moons 1L

let lcm x y = 
    let rec gcd x y = if y = 0L then abs x else gcd y (x % y)
    x * y / (gcd x y)

let big_example = 
    [
        init 0 -8 -10 0
        init 1 5 5 10
        init 2 2 -7 3
        init 3 9 -8 -3 ]

let moons = 
    big_example
    |> List.map (fun m -> m.id, m) |> Map.ofList


[0..3] 
|> List.map (fun id -> periodOf id moons)
|> List.fold lcm 1L

(*
	<x=-9, y=10, z=-1>
	<x=-14, y=-8, z=14>
	<x=1, y=5, z=6>
	<x=-19, y=7, z=8>
*)