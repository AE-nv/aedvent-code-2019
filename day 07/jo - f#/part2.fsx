//7.2: using F# actors (MailBoxProcessors) to represent the different amplifiers.
//The amps print their output to stdout, so some manual maxing of the printed results happens at the end
//#YOLO

type Pointer = int
type Program = int list
type ProgramMode = 
    | Running
    | Finished
type State = { program : Program; mode : ProgramMode; pointer : Pointer; lastOutput : int option }
type ParameterMode =
    | Position
    | Immediate
type Parameter = ParameterMode * int
type Instruction = 
    | Add of Parameter * Parameter * int
    | Multiply of Parameter * Parameter * int
    | Halt
    | Input of int
    | Output of Parameter
    | Equal of Parameter * Parameter * int
    | LessThan of Parameter * Parameter * int
    | JumpIfTrue of Parameter * Parameter
    | JumpIfFalse of Parameter * Parameter
type Message = 
    | ProcessInput of int
    | SetNextAmp of Amplifier
and Amplifier = MailboxProcessor<Message>
and AmpState = { programState : State; next : Amplifier option }
and IO = { input : MailboxProcessor<Message>; output : int -> unit }

let initState program = { program = program; mode = Running; pointer = 0; lastOutput = None }

let zipInfinite otherInfinity one =
    otherInfinity 
    |> Seq.take (one |> Seq.length) 
    |> Seq.toList
    |> List.zip one 
    |> List.map (fun (f,s) -> (s,f))

let parseInstructionAt address program =
    let atPointer = program |> List.skip address
    let reversedInstruction = atPointer |> List.head |> string |> Seq.toList |> List.rev
    let opcode = reversedInstruction
    let modes opLength =
        let parsedModes = reversedInstruction |> List.skip opLength |> List.map (System.Char.GetNumericValue >> int) 
            
        List.append parsedModes (List.replicate 3 0)
        |> List.map (function | 0 -> Position | 1 -> Immediate)

    let parseArgs opSize nbArgs = atPointer |> List.skip 1 |> List.take nbArgs |> zipInfinite (modes opSize) 

    match opcode with
    | '9' :: '9' :: _ -> Halt
    | '8' :: '0' :: _ -> 
        let [a;b;(_,r)] = parseArgs 2 3
        Equal (a,b,r)
    | '8' :: _ -> 
        let [a;b;(_,r)] = parseArgs 1 3
        Equal (a,b,r)
    | '7' :: '0' :: _ -> 
        let [a;b;(_,r)] = parseArgs 2 3
        LessThan (a,b,r)
    | '7' :: _ -> 
        let [a;b;(_,r)] = parseArgs 1 3
        LessThan (a,b,r)
    | '6' :: '0' :: _ -> 
        let [a;b] = parseArgs 2 2
        JumpIfFalse (a,b)
    | '6' :: _ -> 
        let [a;b] = parseArgs 1 2
        JumpIfFalse (a,b)
    | '5' :: '0' :: _ -> 
        let [a;b] = parseArgs 2 2
        JumpIfTrue (a,b)
    | '5' :: _ -> 
        let [a;b] = parseArgs 1 2
        JumpIfTrue (a,b)
    | '1' :: '0' :: _ -> 
        let [a;b;(_, r)] = parseArgs 2 3
        Add (a,b,r)
    | '1' :: _ -> 
        let [a;b;(_, r)] = parseArgs 1 3
        Add (a,b,r)
    | '2' :: '0' :: _ -> 
        let [a;b;(_, r)] = parseArgs 2 3
        Multiply (a,b,r)
    | '2' :: _ -> 
        let [a;b;(_, r)] = parseArgs 1 3
        Multiply (a,b,r)
    | '4' :: '0' :: _ -> 
        let [r] = atPointer |> List.skip 1 |> List.take 1 |> zipInfinite (modes 2)
        Output r
    | '4' :: _ -> 
        let [r] = atPointer |> List.skip 1 |> List.take 1 |> zipInfinite (modes 1)
        Output r
    | '3' :: _ ->
        Input (atPointer |> List.skip 1 |> List.head)
    | err -> failwithf "Unknown opcode: %A" err

let valueAt program parameter = 
    match parameter with
    | Immediate, v -> v
    | Position, p -> program |> List.skip p |> List.head

let rec store value address program =
    match program with
    | _ :: xs when address = 0 -> value :: xs
    | x :: xs -> x :: (store value (address - 1) xs)
    | err -> failwithf "Store out of bounds: %A" err

let pointerIncrement =
    function
    | Halt -> 1
    | Input _ -> 2
    | Output _ -> 2
    | JumpIfTrue _ -> 3
    | JumpIfFalse _ -> 3
    | Add _ -> 4
    | Multiply _ -> 4
    | LessThan _ -> 4
    | Equal _ -> 4

let runSingleInstruction state instruction (io :IO) =
    async {
        let v = valueAt state.program
        match instruction with
        | Halt -> 
            return 
                { state with 
                    mode = Finished
                    pointer = state.pointer + pointerIncrement instruction }
        | Add(a,b,r) ->
            let sum = (v a) + (v b)
            return 
                { state with 
                    program = store sum r state.program
                    pointer = state.pointer + pointerIncrement instruction }
        | Multiply(a,b,r) ->
            let product = (v a) * (v b)
            return  
                { state with 
                    program = store product r state.program
                    pointer = state.pointer + pointerIncrement instruction }
        | Input r ->
            let! (ProcessInput i) = io.input.Receive()
            return
                { state with 
                    program = store i r state.program
                    pointer = state.pointer + pointerIncrement instruction }
        | Output r ->
            let o = v r
            io.output o
            return
                { state with 
                    pointer = state.pointer + pointerIncrement instruction
                    lastOutput = Some o }
        | Equal (a,b,r) ->
            let one = v a
            let other = v b
            let result = if one = other then 1 else 0
            return
                { state with 
                    program = store result r state.program
                    pointer = state.pointer + pointerIncrement instruction }
        | JumpIfTrue (a,r) ->
            return
                { state with 
                    pointer = if v a <> 0 then v r else  state.pointer + pointerIncrement instruction }
        | JumpIfFalse (a,r) ->
            return
                { state with 
                    pointer = if v a = 0 then v r else  state.pointer + pointerIncrement instruction }
        | LessThan (a,b,r) ->
            let result = if (v a) < (v b) then 1 else 0
            return
                { state with 
                    program = store result r state.program
                    pointer = state.pointer + pointerIncrement instruction }
    }

let rec runProgram io state =
    async {
        let! s = state
        match s.mode with
        | Finished -> 
            return s
        | Running -> 
            let i = parseInstructionAt s.pointer s.program
            return! runSingleInstruction s i io |> runProgram io
    }

let rec permute l = 
  let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

  match l with
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

let amplifier program id trial = MailboxProcessor.Start(fun inbox -> 
    let rec init = async {
        let! nextAmp = inbox.Scan(function | SetNextAmp n -> Some (async.Return n) | _ -> None)

        let rec run state = async {
            let output o = 
                state.next |> Option.iter (fun a -> a.Post (ProcessInput o))
            let! next = runProgram ({ input = inbox; output = output }) (async.Return state.programState)
            if id = 'E' && next.mode = Finished 
            then 
                next.lastOutput |> Option.iter (printfn "%d;" )
                return () 
            elif next.mode = Finished then
                return ()
            else 
                return! run { state with programState = next }
        }

        return! run { next = Some nextAmp; programState = initState program }
    }
    init)

let trial program (phases : int list) =
    let a = amplifier program 'A' phases
    let b = amplifier program 'B' phases
    let c = amplifier program 'C' phases
    let d = amplifier program 'D' phases
    let e = amplifier program 'E' phases
    a.Post (Message.SetNextAmp b)
    b.Post (Message.SetNextAmp c)
    c.Post (Message.SetNextAmp d)
    d.Post (Message.SetNextAmp e)
    e.Post (Message.SetNextAmp a)

    a.Post (Message.ProcessInput phases.[0])
    b.Post (Message.ProcessInput phases.[1])
    c.Post (Message.ProcessInput phases.[2])
    d.Post (Message.ProcessInput phases.[3])
    e.Post (Message.ProcessInput phases.[4])

    a.Post (Message.ProcessInput 0)

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
let program : Program = input.Split([|','|]) |> Seq.map int |> Seq.toList
//let program =[3;26;1001;26;-4;26;3;27;1002;27;2;27;1;27;26;27;4;27;1001;28;-1;28;1005;28;6;99;0;0;5]
//let program =[3;52;1001;52;-5;52;3;53;1;52;56;54;1007;54;5;55;1005;55;26;1001;54;-5;54;1105;1;12;1;53;54;53;1008;54;0;55;1001;55;1;55;2;53;55;53;4;53;1001;56;-1;56;1005;56;6;99;0;0;0;0;10]
//trial program [9;8;7;6;5]

let candidates = ([5..9] |> permute)
candidates
|> List.map (fun candidate -> async { return trial program candidate })
|> Async.Parallel
|> Async.RunSynchronously

let results = 
[
    18514496;
    13849224;
    14377602;
    14435012;
    12936226;
    18551396;
    11014860;
    16214538;
    23348592;
    27866668;
    15909704;
    16126386;
    14309962;
    20312472;
    26952288;
    20351432;
    23328096;
    28623736;
    9927326;
    15532800;
    10847778;
    23578138;
    25654772;
    15876656;
    17504858;
    22334386;
    13113052;
    23952078;
    24618050;
    19632748;
    20401214;
    24570874;
    12889058;
    12249742;
    9896574;
    20710752;
    17674632;
    22217296;
    20690256;
    18002504;
    13761068;
    14949794;
    9347680;
    17042338;
    23843444;
    15042042;
    25607604;
    18629276;
    15520496;
    13368028;
    14011438;
    17556114;
    22406154;
    17660652;
    25790034;
    28660632;
    21834392;
    25746954;
    11719296;
    11700848;
    15653340;
    10247620;
    13869720;
    10796514;
    10286580;
    17713592;
    20124496;
    14398116;
    11756424;
    22196800;
    20104000;
    14480844;
    15577492;
    19737254;
    15971184;
    26931792;
    11776920;
    28037736;
    18496048;
    19321282;
    15858208;
    13816404;
    21418420;
    17130490;
    23510466;
    13378284;
    12280494;
    14034014;
    17670908;
    27538288;
    19393050;
    19768014;
    23904940;
    21844716;
    13102796;
    21922576;
    19492572;
    21854972;
    19502828;
    26116836;
    20423790;
    30872528;
    27844092;
    21469684;
    26028712;
    30274224;
    23921318;21844648;

    26018456;
    26079932;
    30286528;
    11025116;
    30860224;
    18063984;
    19601996;
    9366128;
    14400900;
    27517792;
    28074632;
    21910272;
] |> List.max