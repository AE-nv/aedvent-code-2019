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
func store(data []int, param, param_mode, value int) []int {
    if param_mode == 0 {
        data[param] = value
        return data
    }
    panic("you cannot do immidiate with safe opcode")
    return data
}

func add(data []int, pos int, param_modi []int) []int {
    val_a := retrieve(data, data[pos+1], param_modi[0])
    val_b := retrieve(data, data[pos+2], param_modi[1])
    result := val_a + val_b

    return store(data, data[pos+3], param_modi[2], result)
}
func multiply(data []int, pos int, param_modi []int) []int {
    val_a := retrieve(data, data[pos+1], param_modi[0])
    val_b := retrieve(data, data[pos+2], param_modi[1])
    result := val_a * val_b

    return store(data, data[pos+3], param_modi[2], result)
}
func input(data []int, pos int, param_modi []int) []int {
    return store(data, data[pos+1], param_modi[0], input_value)
}
func output(data []int, pos int, param_modi []int) []int {
    val_a := retrieve(data, data[pos+1], param_modi[0])
    fmt.Println("==>>", val_a)
    return data
}
func jit(data []int, pos int, param_modi []int) []int { return data }
func jif(data []int, pos int, param_modi []int) ([]int){ return data}
func lt(data []int, pos int, param_modi []int) ([]int){ return data}
func eq(data []int, pos int, param_modi []int) ([]int){return data}

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
    fmt.Println()
    fmt.Println()
    fmt.Println(data)

    var opcode int
    var param_modi []int

    for pos := 0; opcode != 99 ; {
        opcode, param_modi = parseInstruction(data[pos])

        //fmt.Println("\t", pos, opcode, param_modi, opcode != 99)
        if opcode == 1 {
            data = add(data, pos, param_modi)
            pos += 4
        } else if opcode == 2 {
            data = multiply(data, pos, param_modi)
            pos += 4
        } else if opcode == 3 {
            data = input(data, pos, param_modi)
            pos += 2
        } else if opcode == 4 {
            data = output(data, pos, param_modi)
            pos += 2
        }
        //fmt.Println("\t=>", data, opcode, opcode != 99)
    }
    return data
}


func tests(){
    result := run(parseInput("1,9,10,3,2,3,11,0,99,30,40,50"))
    fmt.Println(result)
    result  = run(parseInput("1,0,0,0,99"))
    fmt.Println(result)
    result  = run(parseInput("2,3,0,3,99"))
    fmt.Println(result)
    result  = run(parseInput("2,4,4,5,99,0"))
    fmt.Println(result)
    result  = run(parseInput("1,1,1,4,99,5,6,0,99"))
    fmt.Println(result)
    result  = run(parseInput("1002,4,3,4,33"))
    fmt.Println(result)
    result  = run(parseInput("1101,100,-1,4,0"))
    fmt.Println(result)
}



var input_value int

func main(){
    tests()
    input_value = 1
    program := parseInput(readInput("input.txt"))
    fmt.Println(run(program))
}
