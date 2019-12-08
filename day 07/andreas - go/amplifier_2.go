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
func possibilities(values []int) [][]int{
    //https://stackoverflow.com/questions/30226438/generate-all-permutations-in-go

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

func parseInput(input string) Amp{
    parts := strings.Split(input, ",")
    codes := make([]int, len(parts))
    for i, x := range parts {
        codes[i], _ = strconv.Atoi(x)
    }
    return Amp{ 0, codes, []int{} }
}

type Amp struct {
    pos int
    data []int
    input_values []int
}

func (p Amp) retrieve(param, param_mode int) int {
    if param_mode == 0 { return p.data[param] }
    return param
}
func (p *Amp) store(param, value int) {
    p.data[param] = value
}
func (p *Amp) inputStream() int{
    value := p.input_values[0]
    fmt.Println("reading", value)
    p.input_values = p.input_values[1:]
    return value
}
func (p *Amp) outputStream(value int) {
    fmt.Println("storing", value)
    p.input_values = append(p.input_values, value)
}

func (p *Amp) add(param_modi []int) {
    val_a := p.retrieve(p.data[p.pos+1], param_modi[0])
    val_b := p.retrieve(p.data[p.pos+2], param_modi[1])
    result := val_a + val_b
    p.store(p.data[p.pos+3], result)
    p.pos+=4
}
func (p *Amp) multiply(param_modi []int) {
    val_a := p.retrieve(p.data[p.pos+1], param_modi[0])
    val_b := p.retrieve(p.data[p.pos+2], param_modi[1])
    result := val_a * val_b
    p.store(p.data[p.pos+3], result)
    p.pos+=4
}
func (p *Amp) input(param_modi []int) {
    value := p.inputStream()
    p.store(p.data[p.pos+1], value)
    p.pos+=2
}
func (p *Amp) output(param_modi []int) int {
    val_a := p.retrieve(p.data[p.pos+1], param_modi[0])
    p.outputStream(val_a)
    p.pos+=2
    return val_a
}
func (p *Amp) jit(param_modi []int) {
    val_a := p.retrieve(p.data[p.pos+1], param_modi[0])
    val_b := p.retrieve(p.data[p.pos+2], param_modi[1])
    if val_a != 0 {
        p.pos = val_b
    }else {
        p.pos+=3
    }
}
func (p *Amp) jif(param_modi []int) {
    val_a := p.retrieve(p.data[p.pos+1], param_modi[0])
    val_b := p.retrieve(p.data[p.pos+2], param_modi[1])
    if val_a == 0 {
        p.pos = val_b
    } else {
        p.pos+=3
    }
}
func (p *Amp) lt(param_modi []int) {
    val_a := p.retrieve(p.data[p.pos+1], param_modi[0])
    val_b := p.retrieve(p.data[p.pos+2], param_modi[1])
    result := 0; if val_a < val_b { result = 1 }
    p.store(p.data[p.pos+3], result)
    p.pos+=4
}
func (p *Amp) eq(param_modi []int) {
    val_a := p.retrieve(p.data[p.pos+1], param_modi[0])
    val_b := p.retrieve(p.data[p.pos+2], param_modi[1])
    result := 0; if val_a == val_b { result = 1 }
    p.store(p.data[p.pos+3], result)
    p.pos+=4
}
func (p *Amp) run() (int, bool) {
    var opcode int
    var param_modi []int

    for opcode != 99 {
        opcode, param_modi = parseInstruction(p.data[p.pos])

        if opcode == 1 {
            p.add(param_modi)
        } else if opcode == 2 {
            p.multiply(param_modi)
        } else if opcode == 3 {
            p.input(param_modi)
        } else if opcode == 4 {
            value := p.output(param_modi)
            return value, false
        } else if opcode == 5 {
            p.jit(param_modi)
        } else if opcode == 6 {
            p.jif(param_modi)
        } else if opcode == 7 {
            p.lt(param_modi)
        } else if opcode == 8 {
            p.eq(param_modi)
        } else if opcode == 99 {
            return 0, true
        } else {
            panic("what just happend?!")
        }
    }
    return 0, true
}

func getOutput(input string, permutation []int) int {
    amps := []Amp{
        parseInput(input),
        parseInput(input),
        parseInput(input),
        parseInput(input),
        parseInput(input),
    }
    for i, setting := range permutation {
        amps[i].outputStream(setting)
    }
    amps[0].outputStream(0)

    var output int
    var done bool
    for i := 0; i < 5 ; i++ {
        output, done = amps[i].run()
        fmt.Println(output, done)

        if i == 5 && done { break }
        amps[(i+1) % 5].outputStream(output)
    }

    return output
}



func search(input string, permutations [][]int) (int, []int) {
    max_perm, max_val := []int{}, -1
    for i, permutation := range permutations {

        output := getOutput(input, permutation)
        if max_val < 0 || output > max_val {
            max_perm = permutation
            max_val = output
        }
        fmt.Println( i, "/", len(permutations), permutation, output)
    }
    return max_val, max_perm
}



func main(){

    input := "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
    fmt.Println(getOutput(input, []int{4,3,2,1,0}))

    input = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
    fmt.Println(getOutput(input, []int{0,1,2,3,4}))

    input = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
    fmt.Println(getOutput(input, []int{1,0,4,3,2}))
    panic("")

    permutations := possibilities([]int{ 5,6,7,8,9 })
    max_val, max_perm := search(input, permutations)
    fmt.Println(max_val, max_perm)
}
