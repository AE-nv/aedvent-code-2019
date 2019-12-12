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
        } else if opcode == 9 {
            p.relativeBase(param_modi)
        } else if opcode == 99 {
            return 0, true
        } else {
            opcode = 99
            //panic("what just happend?!")
        }
    }
    return 0, true
}

type Robot struct{
    x, y int
    direction int
}
func createRobot() Robot{ return Robot{0,0, 0} }
func (r *Robot) move(output int) {
    if output == 0 {
        r.direction++
    } else if output == 1 {
        r.direction--
        if r.direction < 0 { r.direction += 4 }
    }
    r.direction %= 4

    //up, left, down, right
    dx := []int{ 0,-1, 0, 1 }
    dy := []int{-1, 0, 1, 0 }


    r.x += dx[r.direction]
    r.y += dy[r.direction]

}


func createBoard() Board{
    return Board { map[int]map[int]int{} }
}
type Board struct {
    data map[int]map[int]int
}
func (b Board) getValue(x,y int) int {
    if _, ex := b.data[y] ; ! ex { return 0 }

    val, ex := b.data[y][x]
    if ex { return val }
    return 0
}
func (b *Board) setValue(x,y, val int){
    if _, ex := b.data[y]; !ex {
        b.data[y] = map[int]int{}
    }
    b.data[y][x] = val
}
func (b Board) printBoard(rx,ry int) {
    miny, maxy := 0, 0
    minx, maxx := 0, 0

    for y, row := range b.data {
        if y < miny { miny = y }
        if y > maxy { maxy = y }

        for x, _ := range row {
            if x < minx { minx = x }
            if x > maxx { maxx = x }
        }
    }

    for y := miny-1 ; y <= maxy+1 ; y++ {
        for x := minx-1 ; x <= maxx+1 ; x++ {

            if y == ry && x == rx {
                fmt.Print("X")
                continue
            }
            value := b.getValue(x,y)
            if value == 1 {
                fmt.Print("#")
            } else {
                fmt.Print(".")
            }
        }
        fmt.Println()
    }

}
func (b Board) count() int {
    count := 0
    for y, row := range b.data {
        for x, _ := range row {
            if _, ex := b.data[y][x]; ex { count++ }
        }
    }
    return count
}



func main(){

    input := readInput("input.txt")
    program := parseInput(input)
    robot := createRobot()
    board := createBoard()

    program.input_values = append(program.input_values, 0)
    color, done := program.run()
    output, done :=  program.run()
    for ! done  {
        board.setValue(robot.x, robot.y, color)
        robot.move(output)
        program.input_values = append(program.input_values, board.getValue(robot.x, robot.y))

        color, done = program.run()
        output, done =  program.run()
    }
    fmt.Println(board.count())

    /*
    moves := [][2]int{
        [2]int{1,0},
        [2]int{0,0},
        [2]int{1,0},
        [2]int{1,0},
        [2]int{0,1},
        [2]int{1,0},
        [2]int{1,0},
    }

    for _, move := range moves{
        color, output := move[0], move[1]
        board.setValue(robot.x, robot.y, color)
        robot.move(output)
    }
    board.printBoard(robot.x, robot.y)
    */

}
