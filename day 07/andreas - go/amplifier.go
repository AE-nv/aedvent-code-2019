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
    value := input_values[0]
    input_values = input_values[1:]
    return store(data, data[pos+1], value), pos+2
}
func output(data []int, pos int, param_modi []int) ([]int,int) {
    val_a := retrieve(data, data[pos+1], param_modi[0])
    input_values = append(input_values, val_a)
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

        fmt.Println("\t", pos, opcode, param_modi)
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


func getOutput(program []int, settings [5]int) int{
    input_values = []int{settings[0], 0}
    run(program)
    fmt.Println(input_values)

    input_values = append([]int{settings[1]}, input_values...)
    run(program)
    fmt.Println(input_values)

    input_values = append([]int{settings[2]}, input_values...)
    run(program)
    fmt.Println(input_values)

    input_values = append([]int{settings[3]}, input_values...)
    run(program)
    fmt.Println(input_values)

    input_values = append([]int{settings[4]}, input_values...)
    run(program)
    fmt.Println(input_values)

    return input_values[0]
}



var input_values []int

func main(){
    //part1
    program := parseInput("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
    fmt.Println( getOutput(program, [5]int{4,3,2,1,0}) )

    program = parseInput("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0")
    fmt.Println( getOutput(program, [5]int{0,1,2,3,4}) )

    program = parseInput("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")
    fmt.Println( getOutput(program, [5]int{1,0,4,3,2}) )
}
