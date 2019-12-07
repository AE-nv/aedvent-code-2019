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

func retrieve(data []int, param, param_mode int) (int, bool) {
    if param_mode == 0 {
        if param > len(data) { return 0, false }
        return data[param], true
    }
    return param, true
}
func store(data []int, param, param_mode, value int) ([]int, bool){
    if param_mode == 0 {
        if param > len(data) { return data, false }
        data[param] = value
        return data, true
    } else {
        panic("you cannot do immidiate with safe opcode")
    }

    return data, false
}

func add(data []int, pos int, param_modi []int) ([]int, bool) {
    val_a, ok_a := retrieve(data, data[pos+1], param_modi[0])
    if ! ok_a { return data, false }
    val_b, ok_b := retrieve(data, data[pos+2], param_modi[1])
    if ! ok_b { return data, false }

    result := val_a + val_b

    return store(data, data[pos+3], param_modi[2], result)
}
func multiply(data []int, pos int, param_modi []int) ([]int, bool) {
    val_a, ok_a := retrieve(data, data[pos+1], param_modi[0])
    if ! ok_a { return data, false }
    val_b, ok_b := retrieve(data, data[pos+2], param_modi[1])
    if ! ok_b { return data, false }

    result := val_a * val_b

    return store(data, data[pos+3], param_modi[2], result)
}
func input(data []int, pos int, param_modi []int) ([]int, bool){
    return store(data, data[pos+1], param_modi[0], input_value)
}
func output(data []int, pos int, param_modi []int) ([]int, bool){
    val_a, ok := retrieve(data, data[pos+1], param_modi[0])
    if ! ok { return data, false }
    fmt.Println("==>>", val_a)
    return data, true
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
func run(data []int) ([]int, bool) {
    ok := true
    var opcode int
    var param_modi []int
    for pos := 0; ok && opcode != 99 ; {
        opcode, param_modi = parseInstruction(data[pos])

        //fmt.Println("\t", pos, opcode, param_modi)
        if opcode == 1 {
            data, ok = add(data, pos, param_modi)
            pos += 4
        } else if opcode == 2 {
            data, ok = multiply(data, pos, param_modi)
            pos += 4
        } else if opcode == 3 {
            data, ok = input(data, pos, param_modi)
            pos += 2
        } else if opcode == 4 {
            data, ok = output(data, pos, param_modi)
            pos += 2
        }

        //fmt.Println(data, ok)

    }
    return data, ok
}


func part1(){
    result, ok := run(parseInput("1,9,10,3,2,3,11,0,99,30,40,50"))
    fmt.Println(result, ok)
    result, ok = run(parseInput("1,0,0,0,99"))
    fmt.Println(result, ok)
    result, ok = run(parseInput("2,3,0,3,99"))
    fmt.Println(result, ok)
    result, ok = run(parseInput("2,4,4,5,99,0"))
    fmt.Println(result, ok)
    result, ok = run(parseInput("1,1,1,4,99,5,6,0,99"))
    fmt.Println(result, ok)

}



var input_value int

func main(){
    input_value = 1
    part1()
}
