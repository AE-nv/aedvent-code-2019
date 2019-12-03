package main

import (
    "fmt"
    "strings"
    "strconv"
    "io/ioutil"
)


func abs(val int) int{
    if val < 0 { return -val }
    return val
}
type Instruction struct{
    direction byte
    steps int
}
func parseInstruction(part string) Instruction{
    instr := new(Instruction)
    instr.direction = part[0]
    instr.steps,_ = strconv.Atoi(part[1:])
    return *instr
}
func parseInput(input string) [][]Instruction{
    lines := strings.Split(input, "\n")
    wires := make([][]Instruction, len(lines))
    for i, line := range lines {
        if len(line) == 0 {  continue }
        parts := strings.Split(line, ",")
        instructions := make([]Instruction, len(parts))
        for j, part := range parts{
            instructions[j] = parseInstruction(part)
        }
        wires[i] = instructions
    }

    return wires
}

type Board struct{
    data map[int]map[int]map[int]int
    crossings [][2]int
    wire_count int
}
func (b Board) print(){
    minx, miny, maxx, maxy := 0,0,0,0
    for y, val := range b.data {

        if y < miny {
            miny = y
        } else if y > maxy {
            maxy = y
        }

        for x, _ := range val{
            if x > maxx {
                maxx = x
            } else if x > maxx {
                maxx = x
            }
        }
    }

    for y := maxy + 1; y > miny -1 ; y-- {
        for x := minx -1 ; x < maxx +1; x++ {
            if _, ok := b.data[y]; !ok {
                fmt.Print(".")
            } else if _, ok := b.data[y][x]; !ok {
                fmt.Print(".")
            } else {
                fmt.Print(len(b.data[y][x]))
            }
        }

        fmt.Println()
    }
}
func (b *Board) addWire(wire []Instruction) {
    y, x := 0, 0
    distance := 0
    for _, instr := range wire{
        dy, dx := 0, 0
        if instr.direction == 'U'{
            dy = 1
        } else if instr.direction == 'D' {
            dy = -1
        } else if instr.direction == 'R' {
            dx = 1
        } else if instr.direction == 'L' {
            dx = -1
        }

        for i := 0; i < instr.steps; i++ {
            distance++
            y += dy
            x += dx

            if _, ok := b.data[y]; !ok {
                b.data[y] = make(map[int]map[int]int)
            }
            if _, ok := b.data[y][x]; !ok {
                b.data[y][x] = make(map[int]int)
                //ensures we don't trigger the intersection below
                b.data[y][x][b.wire_count] = distance
            }

            if _, ok := b.data[y][x][b.wire_count]; !ok {
                b.data[y][x][b.wire_count] = distance
                b.crossings = append(b.crossings, [2]int{y,x})
                //fmt.Println("crossing", y,x, b.data[y][x])
            }
        }
    }

    b.wire_count++
}

func getDistancePart1(input string) int{
    board := Board{
        data: make(map[int]map[int]map[int]int),
        crossings: [][2]int{},
        wire_count: 0,
    }

    wires := parseInput(input)
    for _, wire := range wires {
        board.addWire(wire)
        //board.print()
    }

    min_distance := abs(board.crossings[0][0]) + abs(board.crossings[0][1])
    for _, crossing := range board.crossings[1:] {
        distance := abs(crossing[0]) + abs(crossing[1])
        //fmt.Println(crossing, "=>", distance)
        if distance < min_distance{
            min_distance = distance
        }
    }

    return min_distance
}

func getDistancePart2(input string) int{
    board := Board{
        data: make(map[int]map[int]map[int]int),
        crossings: [][2]int{},
        wire_count: 0,
    }

    wires := parseInput(input)
    for _, wire := range wires {
        board.addWire(wire)
    }
    //board.print()

    min_distance := -1
    for _, crossing := range board.crossings {

        curr_distance := 0
        for _, distance := range board.data[crossing[0]][crossing[1]] {
            curr_distance += distance
        }

        if min_distance < 0 || curr_distance < min_distance {
            min_distance = curr_distance
        }
    }

    return min_distance
}



func readInput(fname string) string{
    text, _ := ioutil.ReadFile(fname)
    return string(text)
}


func main(){
    input := `R8,U5,L5,D3
U7,R6,D4,L4`
    fmt.Println("part 1:", getDistancePart1(input))
    fmt.Println("part 2:", getDistancePart2(input))

    input = `R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83`
    fmt.Println("part 1:", getDistancePart1(input))
    fmt.Println("part 2:", getDistancePart2(input))

    input = `R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7`
    fmt.Println("part 1:", getDistancePart1(input))
    fmt.Println("part 2:", getDistancePart2(input))

    input = readInput("input.txt")
    fmt.Println("part 1:", getDistancePart1(input))
    fmt.Println("part 2:", getDistancePart2(input))
}
