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

func add(data []int, pos int) ([]int, bool) {
    pos_a := data[pos+1]
    if pos_a >= len(data) { return data, false }

    pos_b := data[pos+2]
    if pos_b >= len(data) { return data, false }

    pos_result := data[pos+3]
    if pos_result >= len(data) { return data, false }

    data[pos_result] = data[pos_a] + data[pos_b]

    return data, true
}

func multiply(data []int, pos int) ([]int, bool) {
    pos_a := data[pos+1]
    if pos_a >= len(data) { return data, false }

    pos_b := data[pos+2]
    if pos_b >= len(data) { return data, false }

    pos_result := data[pos+3]
    if pos_result >= len(data) { return data, false }

    data[pos_result] = data[pos_a] * data[pos_b]

    return data, true
}

func run(data []int) ([]int, bool) {
    ok := true
    for pos, value := 0, data[0]; ok && value != 99 ; value = data[pos] {
        if value == 1 {
            data, ok = add(data, pos)
        } else if value == 2 {
            data, ok = multiply(data, pos)
        }
        pos += 4
    }
    return data, ok
}


func search(data []int, noun,verb int) (int, bool){
    data[1] = noun
    data[2] = verb
    result, ok := run(data)
    return  result[0], ok
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

    data := parseInput(readInput("input.txt"))
    final, _ := search(data, 12, 2)
    fmt.Println(final)
}

func part2() (int, int){
    src_data := parseInput(readInput("input.txt"))
    data := make([]int, len(src_data))

    for noun := 0; noun < 100; noun++ {
        for verb := 0; verb < 100; verb++{
            copy(data, src_data) //create fresh copy

            result, ok := search(data, noun, verb)
            if ok && result == 19690720 {
                return noun, verb
            }
        }
    }
    return -1, -1
}



func main(){
    part1()
    noun, verb := part2()
    fmt.Println( noun*100+verb)
}
