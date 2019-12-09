#r @"..\..\dependencies\jo\paket\packages\Unquote\lib\net45\Unquote.dll"
open Swensen.Unquote

type Pointer = int64
type Program = int64 list
type ProgramMode = 
    | Running
    | Finished
type State = { program : Program; mode : ProgramMode; pointer : Pointer; input : int64 list; output: int64 list; relativeBase : int64 }
type ParameterMode =
    | Position
    | Immediate
    | Relative
type Parameter = ParameterMode * int64
type Instruction = 
    | Add of Parameter * Parameter * int64
    | Multiply of Parameter * Parameter * int64
    | Halt
    | Input of Parameter
    | Output of Parameter
    | Equal of Parameter * Parameter * int64
    | LessThan of Parameter * Parameter * int64
    | JumpIfTrue of Parameter * Parameter
    | JumpIfFalse of Parameter * Parameter
    | AdjustRelativeBase of Parameter

let zipInfinite otherInfinity one =
    otherInfinity 
    |> Seq.take (one |> Seq.length) 
    |> Seq.toList
    |> List.zip one 
    |> List.map (fun (f,s) -> (s,f))

let parseInstructionAt (address : int64) program =
    let atPointer = program |> List.skip (int address)
    let reversedInstruction = atPointer |> List.head |> string |> Seq.toList |> List.rev
    let opcode = reversedInstruction
    let modes opLength =
        let parsedModes = reversedInstruction |> List.skip opLength |> List.map (System.Char.GetNumericValue >> int) 
            
        List.append parsedModes (List.replicate 3 0)
        |> List.map (function | 0 -> Position | 1 -> Immediate | 2 -> Relative)

    let parseArgs opSize nbArgs = atPointer |> List.skip 1 |> List.take nbArgs |> zipInfinite (modes opSize) 

    match opcode with
    | '9' :: '9' :: _ -> Halt
    | '9' :: '0' :: _ -> 
        let [a] = parseArgs 2 1
        AdjustRelativeBase a
    | '9' :: _ -> 
        let [a] = parseArgs 1 1
        AdjustRelativeBase a
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
    | '3' :: '0' :: _ ->
        let [a] = parseArgs 2 1
        Input a
    | '3' :: _ ->
        let [a] = parseArgs 1 1
        Input a
    | err -> failwithf "Unknown opcode: %A" err

let valueAt program (relativeBase : int64) (parameter : ParameterMode * int64) = 
    match parameter with
    | Immediate, v -> v
    | Position, p -> 
        let offset = p |> int
        if offset >= (program |> List.length)
        then 0L
        else program |> List.skip offset |> List.head
    | Relative, r -> 
        let offset = relativeBase + r |> int
        if offset >= (program |> List.length)
        then 0L
        else program |> List.skip offset |> List.head

let rec store value address program =
    match program with
    | _ :: xs when address = 0L -> value :: xs
    | x :: xs -> x :: (store value (address - 1L) xs)
    | [] when address = 0L -> [value]
    | [] -> 0L :: (store value (address - 1L) [])

let pointerIncrement =
    function
    | Halt -> 1L
    | Input _ -> 2L
    | Output _ -> 2L
    | AdjustRelativeBase _ -> 2L
    | JumpIfTrue _ -> 3L
    | JumpIfFalse _ -> 3L
    | Add _ -> 4L
    | Multiply _ -> 4L
    | LessThan _ -> 4L
    | Equal _ -> 4L

let runSingleInstruction state instruction =
    let v = valueAt state.program state.relativeBase
    match instruction with
    | Halt -> 
        { state with 
            mode = Finished
            pointer = state.pointer + pointerIncrement instruction }
    | Add(a,b,r) ->
        let sum = (v a) + (v b)
        { state with 
            program = store sum r state.program
            pointer = state.pointer + pointerIncrement instruction }
    | Multiply(a,b,r) ->
        let product = (v a) * (v b)
        { state with 
            program = store product r state.program
            pointer = state.pointer + pointerIncrement instruction }
    | Input r ->
        let i::is = state.input
        let destination = v r
        { state with 
            program = store i destination state.program
            pointer = state.pointer + pointerIncrement instruction
            input = is }
    | Output r ->
        let o = v r
        { state with 
            pointer = state.pointer + pointerIncrement instruction
            output = o :: state.output }
    | Equal (a,b,r) ->
        let one = v a
        let other = v b
        let result = if one = other then 1L else 0L
        { state with 
            program = store result r state.program
            pointer = state.pointer + pointerIncrement instruction }
    | JumpIfTrue (a,r) ->
        { state with 
            pointer = if v a <> 0L then v r else  state.pointer + pointerIncrement instruction }
    | JumpIfFalse (a,r) ->
        { state with 
            pointer = if v a = 0L then v r else  state.pointer + pointerIncrement instruction }
    | LessThan (a,b,r) ->
        let result = if (v a) < (v b) then 1L else 0L
        { state with 
            program = store result r state.program
            pointer = state.pointer + pointerIncrement instruction }
    | AdjustRelativeBase a ->
        { state with
            relativeBase = state.relativeBase + (v a)
            pointer = state.pointer + pointerIncrement instruction }

let rec run (state : State) : State =
    match state.mode with
    | Finished -> state
    | Running -> 
        let i = parseInstructionAt state.pointer state.program
        runSingleInstruction state i
        |> run

let initState inputs program =
    { program = program; mode = Running; pointer = 0L; input = inputs; output = []; relativeBase = 0L }

printf "Testing..."

//Parameter modes
test <@ parseInstructionAt 0L [10001L;9L;10L;3L;2L;3L;11L;0L;99L;30L;40L;50L] = Add((Position, 9L),(Position, 10L),3L) @>
test <@ parseInstructionAt 4L [10001L;9L;10L;3L;10002L;3L;11L;0L;99L;30L;40L;50L] = Multiply((Position, 3L),(Position, 11L),0L) @>
test <@ parseInstructionAt 0L [1002L;4L;3L;4L;33L] = Multiply ((Position, 4L),(Immediate, 3L),4L) @>
test <@ parseInstructionAt 0L [2002L;4L;3L;4L;33L] = Multiply ((Position, 4L),(Relative, 3L),4L) @>
test <@ parseInstructionAt 0L [3L;225L] = Input (Position, 225L) @>
test <@ parseInstructionAt 0L [203L;225L] = Input (Relative, 225L) @>
test <@ parseInstructionAt 0L [4L;225L] = Output (Position, 225L) @>
test <@ parseInstructionAt 0L [104L;225L] = Output (Immediate, 225L) @>
test <@ parseInstructionAt 0L [8L;9L;10L;9L] = Equal ((Position, 9L),(Position, 10L),9L) @>
test <@ parseInstructionAt 0L [1108L;7L;8L;3L] = Equal ((Immediate, 7L),(Immediate, 8L),3L) @>
test <@ parseInstructionAt 0L [109L;33L] = AdjustRelativeBase (Immediate, 33L) @>
test <@ parseInstructionAt 0L [209L;33L] = AdjustRelativeBase (Relative, 33L) @>

//Running instructions
test <@ 
        let s = runSingleInstruction (initState [] [1L;9L;10L;3L;2L;3L;11L;0L;99L;30L;40L;50L])  (Add((Position, 9L),(Position, 10L),3L)) 
        s.program = [1L; 9L; 10L; 70L; 2L; 3L; 11L; 0L; 99L; 30L; 40L; 50L] && s.pointer = 4L @>
test <@ 
        let init = { initState [] [1L;9L;10L;3L;2L;3L;11L;0L;99L;30L;40L;50L] with relativeBase = 3L }
        let s = runSingleInstruction init (Add((Position, 9L),(Relative, 1L),3L)) 
        s.program = [1L; 9L; 10L; 32L; 2L; 3L; 11L; 0L; 99L; 30L; 40L; 50L] && s.pointer = 4L @>
test <@ 
        let s = runSingleInstruction (initState [] [1337L]) (AdjustRelativeBase (Position, 0L))
        s.program = [1337L] && s.pointer = 2L && s.relativeBase = 1337L @>
test <@ 
        let init = { initState [1337L] [0L;1L;2L] with relativeBase = 1L }
        let s = runSingleInstruction init (Input (Relative, 2L)) 
        s.program = [0L;1L;2L;1337L] @>

//Infinite tape
//initialized to 0
test <@
        let s = runSingleInstruction (initState [] [1337L]) (Output (Position, 1L))
        s.output = [0L] @>
//can store "out of bounds"
test <@
        let s = runSingleInstruction (initState [666L] [1337L]) (Input (Immediate, 1L))
        s.program = [1337L;666L] @>
//Running programs
test <@ initState [] [10001L;9L;10L;3L;10002L;3L;11L;0L;99L;30L;40L;50L] |> run |> (fun s -> (s.mode, s.program)) = (Finished, [3500L; 9L; 10L; 70L; 10002L; 3L; 11L; 0L; 99L; 30L; 40L; 50L])  @>

//Variable instruction length/IP increment
test <@ pointerIncrement (Add((Position, 9L),(Position, 10L),3L)) = 4L @>
test <@ pointerIncrement (Input (Immediate, 1L)) = 2L @>
test <@ pointerIncrement (Output (Position, 1L)) = 2L @>
test <@ pointerIncrement Halt = 1L @>

test <@ valueAt [0L;1L;2L;3L] 1L (Relative, 2L) = 3L @>

//IO
test <@ initState [1337L;666L] [3L;1L;4L;1L;99L] |> run |> (fun s -> s.input, s.output) = ([666L], [1337L]) @>

//Examples
test <@ 
        let p = [109L;1L;204L;-1L;1001L;100L;1L;100L;1008L;100L;16L;101L;1006L;101L;0L;99L]
        let s = initState [] p |> run 
        s.output |> List.rev = p @>
test <@ 
        let s = initState [] [1102L;34915192L;34915192L;7L;4L;7L;99L;0L] |> run 
        s.output = [1219070632396864L] (*A VERY LARGE NUMBER*) @>
test <@ 
        let s = initState [] [104L;1125899906842624L;99L] |> run 
        s.output = [1125899906842624L] (*A VERY LARGE NUMBER*) @>

printfn "..done!"

let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "\input.txt")
let p : Program = input.Split([|','|]) |> Seq.map int64 |> Seq.toList

initState [1L] p |> run