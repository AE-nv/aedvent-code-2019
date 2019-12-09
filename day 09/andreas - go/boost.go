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
    codes := map[int]int{}
    for i, x := range parts {
        codes[i], _ = strconv.Atoi(x)
    }
    return Amp{ 0, 0, codes, []int{} }
}

type Amp struct {
    pos int
    relative_base int
    data map[int]int
    input_values []int
}

func (p Amp) retrieve(param, param_mode int) int {
    if param_mode == 0 {
        return p.data[param]
    } else if param_mode == 1 {
        return param
    } else if param_mode == 2 {
        return p.data[ p.relative_base + param ]
    }
    return param
}
func (p *Amp) store(param, value, param_mode int) {
    if param_mode == 0 {
        p.data[param] = value
    } else if param_mode == 1{
        fmt.Println("you cannot write to param mode 1, store operation will be ignored")
    } else {
        p.data[ p.relative_base + param ] = value
    }
}
func (p *Amp) inputStream() int{
    value := p.input_values[0]
    p.input_values = p.input_values[1:]
    return value
}

func (p *Amp) add(param_modi []int) {
    val_a := p.retrieve(p.data[p.pos+1], param_modi[0])
    val_b := p.retrieve(p.data[p.pos+2], param_modi[1])
    result := val_a + val_b
    p.store(p.data[p.pos+3], result, param_modi[2])
    p.pos+=4
}
func (p *Amp) multiply(param_modi []int) {
    val_a := p.retrieve(p.data[p.pos+1], param_modi[0])
    val_b := p.retrieve(p.data[p.pos+2], param_modi[1])
    result := val_a * val_b
    p.store(p.data[p.pos+3], result, param_modi[2])
    p.pos+=4
}
func (p *Amp) input(param_modi []int) {
    value := p.inputStream()
    p.store(p.data[p.pos+1], value, param_modi[0])
    p.pos+=2
}
func (p *Amp) output(param_modi []int) int {
    val_a := p.retrieve(p.data[p.pos+1], param_modi[0])
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
    p.store(p.data[p.pos+3], result, param_modi[2])
    p.pos+=4
}
func (p *Amp) eq(param_modi []int) {
    val_a := p.retrieve(p.data[p.pos+1], param_modi[0])
    val_b := p.retrieve(p.data[p.pos+2], param_modi[1])
    result := 0; if val_a == val_b { result = 1 }
    p.store(p.data[p.pos+3], result, param_modi[2])
    p.pos+=4
}
func (p *Amp) relativeBase(param_modi []int){
    val_a := p.retrieve(p.data[p.pos+1], param_modi[0])
    p.relative_base += val_a
    p.pos += 2
}

func (p *Amp) run() (int, bool) {
    var opcode int
    var param_modi []int

    for opcode != 99 {
        opcode, param_modi = parseInstruction(p.data[p.pos])

        fmt.Println(p.data[p.pos], "=>", p.pos, opcode, param_modi, p.input_values)

        if opcode == 1 {
            p.add(param_modi)
        } else if opcode == 2 {
            p.multiply(param_modi)
        } else if opcode == 3 {
            p.input(param_modi)
        } else if opcode == 4 {
            value := p.output(param_modi)
            p.input_values = append(p.input_values, value)
            fmt.Println(value)
        } else if opcode == 5 {
            p.jit(param_modi)
        } else if opcode == 6 {
            p.jif(param_modi)
        } else if opcode == 7 {
            p.lt(param_modi)
        } else if opcode == 8 {
            p.eq(param_modi)
        } else if opcode == 9 {
            p.relativeBase(param_modi)
        } else if opcode == 99 {
            return 0, true
        } else {
            fmt.Println("invalid opcode")
            opcode = 99
            //panic("what just happend?!")
        }
    }
    return 0, true
}




func main(){
    /*
    program := parseInput("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")
    fmt.Println(program.run())

    program = parseInput("1102,34915192,34915192,7,4,7,99,0")
    fmt.Println(program.run())

    program = parseInput("104,1125899906842624,99")
    fmt.Println(program.run())

    input := readInput("input.txt")
    program := parseInput(input)
    program.input_values = append(program.input_values, 1)
    fmt.Println(program.run())
    */

    input := readInput("input.txt")
    program := parseInput(input)
    program.input_values = append(program.input_values, 2)
    fmt.Println(program.run())

}
