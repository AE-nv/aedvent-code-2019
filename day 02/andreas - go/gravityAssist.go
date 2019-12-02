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

func add(data []int, pos int) []int {
    pos_a := data[pos+1]
    pos_b := data[pos+2]
    pos_result := data[pos+3]
    data[pos_result] = data[pos_a] + data[pos_b]

    return data
}

func multiply(data []int, pos int) []int {
    pos_a := data[pos+1]
    pos_b := data[pos+2]
    pos_result := data[pos+3]
    data[pos_result] = data[pos_a] * data[pos_b]

    return data
}

func run(data []int){
    for pos, value := 0, data[0]; value != 99 ; value = data[pos] {
        if value == 1 {
            data = add(data, pos)
        } else if value == 2 {
            data = multiply(data, pos)
        }
        pos += 4
    }

    fmt.Println(data)
}


func main(){

    input := "1,9,10,3,2,3,11,0,99,30,40,50"
    data := parseInput(input)
    run(data)

    run(parseInput("1,0,0,0,99"))
    run(parseInput("2,3,0,3,99"))
    run(parseInput("2,4,4,5,99,0"))
    run(parseInput("1,1,1,4,99,5,6,0,99"))


    fname := "input.txt"
    input = readInput(fname)
    data = parseInput(input)
    data[1] = 12
    data[2] = 2
    run(data)
}
