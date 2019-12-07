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
func possibilities(size int) [][]int{
    //https://stackoverflow.com/questions/30226438/generate-all-permutations-in-go
    values := make([]int, size)
    for i := 0 ; i < size; i++ { values[i] = i }

    var helper func([]int, int)
    res := [][]int{}

    helper = func(arr []int, n int){
        if n == 1{
            tmp := make([]int, len(arr))
            copy(tmp, arr)
            res = append(res, tmp)
        } else {
            for i := 0; i < n; i++{
                helper(arr, n - 1)
                if n % 2 == 1{
                    tmp := arr[i]
                    arr[i] = arr[n - 1]
                    arr[n - 1] = tmp
                } else {
                    tmp := arr[0]
                    arr[0] = arr[n - 1]
                    arr[n - 1] = tmp
                }
            }
        }
    }
    helper(values, len(values))
    return res
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

func parseInput(input string) Program{
    parts := strings.Split(input, ",")
    codes := make([]int, len(parts))
    for i, x := range parts {
        codes[i], _ = strconv.Atoi(x)
    }
    return Program{ codes, []int{} }
}

type Program struct {
    data []int
    input_values []int
}

func (p Program) retrieve(param, param_mode int) int {
    if param_mode == 0 { return p.data[param] }
    return param
}
func (p *Program) store(param, value int) {
    p.data[param] = value
}
func (p *Program) inputStream() int{
    value := p.input_values[0]
    p.input_values = p.input_values[1:]
    return value
}
func (p *Program) outputStream(val int) {
    p.input_values = append(p.input_values, val)
}

func (p *Program) add(pos int, param_modi []int) int {
    val_a := p.retrieve(p.data[pos+1], param_modi[0])
    val_b := p.retrieve(p.data[pos+2], param_modi[1])
    result := val_a + val_b
    p.store(p.data[pos+3], result)
    return pos+4
}
func (p *Program) multiply(pos int, param_modi []int) int {
    val_a := p.retrieve(p.data[pos+1], param_modi[0])
    val_b := p.retrieve(p.data[pos+2], param_modi[1])
    result := val_a * val_b
    p.store(p.data[pos+3], result)
    return pos+4
}
func (p *Program) input(pos int, param_modi []int) int {
    value := p.inputStream()
    p.store(p.data[pos+1], value)
    return pos+2
}
func (p *Program) output(pos int, param_modi []int) int {
    val_a := p.retrieve(p.data[pos+1], param_modi[0])
    p.outputStream(val_a)
    return pos+2
}
func (p *Program) jit(pos int, param_modi []int) int {
    val_a := p.retrieve(p.data[pos+1], param_modi[0])
    val_b := p.retrieve(p.data[pos+2], param_modi[1])
    if val_a != 0 { return val_b }
    return pos+3
}
func (p *Program) jif(pos int, param_modi []int) int {
    val_a := p.retrieve(p.data[pos+1], param_modi[0])
    val_b := p.retrieve(p.data[pos+2], param_modi[1])
    if val_a == 0 { return val_b }
    return pos+3
}
func (p *Program) lt(pos int, param_modi []int) int {
    val_a := p.retrieve(p.data[pos+1], param_modi[0])
    val_b := p.retrieve(p.data[pos+2], param_modi[1])
    result := 0; if val_a < val_b { result = 1 }
    p.store(p.data[pos+3], result)
    return pos+4
}
func (p *Program) eq(pos int, param_modi []int) int {
    val_a := p.retrieve(p.data[pos+1], param_modi[0])
    val_b := p.retrieve(p.data[pos+2], param_modi[1])
    result := 0; if val_a == val_b { result = 1 }
    p.store(p.data[pos+3], result)
    return pos+4
}
func (p *Program) run() {
    pos := 0
    var opcode int
    var param_modi []int

    for opcode != 99 {
        opcode, param_modi = parseInstruction(p.data[pos])

        if opcode == 1 {
            pos = p.add(pos, param_modi)
        } else if opcode == 2 {
            pos = p.multiply(pos, param_modi)
        } else if opcode == 3 {
            pos = p.input(pos, param_modi)
        } else if opcode == 4 {
            pos = p.output(pos, param_modi)
        } else if opcode == 5 {
            pos = p.jit(pos, param_modi)
        } else if opcode == 6 {
            pos = p.jif(pos, param_modi)
        } else if opcode == 7 {
            pos = p.lt(pos, param_modi)
        } else if opcode == 8 {
            pos = p.eq(pos, param_modi)
        } else if opcode == 99 {
            //will quit eventually
        } else {
            panic("what just happend?!")
        }
    }
}



func (p *Program) getOutput(settings []int) int{
    p.input_values = []int{settings[0], 0}
    p.run()
    p.input_values = append([]int{settings[1]}, p.input_values...)
    p.run()
    p.input_values = append([]int{settings[2]}, p.input_values...)
    p.run()
    p.input_values = append([]int{settings[3]}, p.input_values...)
    p.run()
    p.input_values = append([]int{settings[4]}, p.input_values...)
    p.run()
    return p.input_values[0]
}

func search(input string) (int, []int) {
    permutations := possibilities(5)
    max_perm, max_val := []int{}, -1
    for _, permutation := range permutations {
        program := parseInput(input)
        value := program.getOutput(permutation)
        //fmt.Println( i, "/", len(permutations))

        if max_val < 0 || value > max_val {
            max_perm = permutation
            max_val = value
        }

    }
    return max_val, max_perm
}



func main(){
    /*
    program := parseInput("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
    fmt.Println( getOutput(program, []int{4,3,2,1,0}) )

    program = parseInput("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0")
    fmt.Println( getOutput(program, []int{0,1,2,3,4}) )

    program = parseInput("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")
    fmt.Println( getOutput(program, []int{1,0,4,3,2}) )

    input := "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
    input  = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
    input  = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
    */
    input := readInput("input.txt")
    max_val, max_perm := search(input)
    fmt.Println(max_val, max_perm)
}
