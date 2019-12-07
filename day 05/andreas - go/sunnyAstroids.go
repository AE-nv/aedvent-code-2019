package main

import (
    "fmt"
    "strings"
    "strconv"
    "io/ioutil"
)

func readInput(fname string) string{
    text, _ := ioutil.ReadFile(fname)
    return string(text)
}

func parseInput(input string) []int{
    parts := strings.Split(input, ",")
    codes := make([]int, len(parts))
    for i, x := range parts {
        codes[i], _ = strconv.Atoi(x)
    }
    return codes
}

func retrieve(data []int, param, param_mode int) int {
    if param_mode == 0 { return data[param] }
    return param
}
func store(data []int, param, value int) []int {
    data[param] = value
    return data
}

func add(data []int, pos int, param_modi []int) ([]int,int) {
    val_a := retrieve(data, data[pos+1], param_modi[0])
    val_b := retrieve(data, data[pos+2], param_modi[1])
    result := val_a + val_b
    return store(data, data[pos+3], result), pos+4
}
func multiply(data []int, pos int, param_modi []int) ([]int,int) {
    val_a := retrieve(data, data[pos+1], param_modi[0])
    val_b := retrieve(data, data[pos+2], param_modi[1])
    result := val_a * val_b
    return store(data, data[pos+3], result), pos+4
}
func input(data []int, pos int, param_modi []int) ([]int,int) {
    return store(data, data[pos+1], input_value), pos+2
}
func output(data []int, pos int, param_modi []int) ([]int,int) {
    val_a := retrieve(data, data[pos+1], param_modi[0])
    fmt.Println("==>>", val_a)
    return data, pos+2
}
func jit(data []int, pos int, param_modi []int) ([]int,int) {
    val_a := retrieve(data, data[pos+1], param_modi[0])
    val_b := retrieve(data, data[pos+2], param_modi[1])
    if val_a != 0 { return data, val_b }
    return data, pos+3
}
func jif(data []int, pos int, param_modi []int) ([]int,int) {
    val_a := retrieve(data, data[pos+1], param_modi[0])
    val_b := retrieve(data, data[pos+2], param_modi[1])
    if val_a == 0 { return data, val_b }
    return data, pos+3
}
func lt(data []int, pos int, param_modi []int) ([]int,int) {
    val_a := retrieve(data, data[pos+1], param_modi[0])
    val_b := retrieve(data, data[pos+2], param_modi[1])
    result := 0; if val_a < val_b { result = 1 }
    return store(data, data[pos+3], result), pos+4
}
func eq(data []int, pos int, param_modi []int) ([]int,int) {
    val_a := retrieve(data, data[pos+1], param_modi[0])
    val_b := retrieve(data, data[pos+2], param_modi[1])
    result := 0; if val_a == val_b { result = 1 }
    return store(data, data[pos+3], result), pos+4
}

func parseInstruction(code int) (int, []int){
    opcode := code % 100
    code /= 100
    parameter_modi := []int{}
    for ; code > 0 ; code/= 10 {
        parameter_modi = append(parameter_modi, code%10)
    }

    for i := len(parameter_modi) ; i < 3; i++ {
        parameter_modi = append(parameter_modi, 0)
    }
    return opcode, parameter_modi
}
func run(data []int) ([]int) {
    fmt.Println()
    fmt.Println(data)
    pos := 0
    var opcode int
    var param_modi []int

    for opcode != 99 {
        opcode, param_modi = parseInstruction(data[pos])

        //fmt.Println("\t", pos, opcode, param_modi)
        if opcode == 1 {
            data, pos = add(data, pos, param_modi)
        } else if opcode == 2 {
            data, pos = multiply(data, pos, param_modi)
        } else if opcode == 3 {
            data, pos = input(data, pos, param_modi)
        } else if opcode == 4 {
            data, pos = output(data, pos, param_modi)
        } else if opcode == 5 {
            data, pos = jit(data, pos, param_modi)
        } else if opcode == 6 {
            data, pos = jif(data, pos, param_modi)
        } else if opcode == 7 {
            data, pos = lt(data, pos, param_modi)
        } else if opcode == 8 {
            data, pos = eq(data, pos, param_modi)
        } else if opcode == 99 {
            //will quit eventually
        } else {
            panic("what just happend?!")
        }
        //fmt.Println("\t=>", data, pos)
    }
    return data
}


func tests_day2(){
    result := run(parseInput("1,0,0,0,99"))
    fmt.Println(result)
    result  = run(parseInput("2,3,0,3,99"))
    fmt.Println(result)
    result  = run(parseInput("2,4,4,5,99,0"))
    fmt.Println(result)
    result  = run(parseInput("1,1,1,4,99,5,6,0,99"))
    fmt.Println(result)
    result  = run(parseInput("1,9,10,3,2,3,11,0,99,30,40,50"))
    fmt.Println(result)
}

func tests_part1(){
    result := run(parseInput("1002,4,3,4,33"))
    fmt.Println(result)
    result  = run(parseInput("1101,100,-1,4,0"))
    fmt.Println(result)
}

func tests_part2(){
    fmt.Println("part 2")
    input_value = 7
    fmt.Println(run(parseInput("3,9,8,9,10,9,4,9,99,-1,8")))
    fmt.Println(run(parseInput("3,9,7,9,10,9,4,9,99,-1,8")))
    fmt.Println(run(parseInput("3,3,1108,-1,8,3,4,3,99")))
    fmt.Println(run(parseInput("3,3,1107,-1,8,3,4,3,99")))

    input_value = 8
    fmt.Println(run(parseInput("3,9,8,9,10,9,4,9,99,-1,8")))
    fmt.Println(run(parseInput("3,9,7,9,10,9,4,9,99,-1,8")))
    fmt.Println(run(parseInput("3,3,1108,-1,8,3,4,3,99")))
    fmt.Println(run(parseInput("3,3,1107,-1,8,3,4,3,99")))
    fmt.Println(run(parseInput("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")))
    fmt.Println(run(parseInput("3,3,1105,-1,9,1101,0,0,12,4,12,99,1")))

    fmt.Println(run(parseInput("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")))
    fmt.Println(run(parseInput("3,3,1105,-1,9,1101,0,0,12,4,12,99,1")))
    input_value = 0
    fmt.Println(run(parseInput("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")))
    fmt.Println(run(parseInput("3,3,1105,-1,9,1101,0,0,12,4,12,99,1")))

    fmt.Println(run(parseInput("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99")))
}



var input_value int

func main(){
    //TODO put this in testing framework :o
    //tests_day2()
    //tests_part1()
    //tests_part2()

    //part1
    input_value = 1
    program := parseInput(readInput("input.txt"))
    run(program)

    //part2
    input_value = 5
    program = parseInput(readInput("input.txt"))
    run(program)
}
