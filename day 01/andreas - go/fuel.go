package main

import (
	"fmt"
    "io/ioutil"
    "strings"
    "strconv"
)


func fuel(mass int) int{
	return (mass/3)-2
}

func readInput(fname string) *[]int{
    text, _ := ioutil.ReadFile(fname)
    lines := strings.Split(string(text), "\n")
    lines = lines[:len(lines)-1] //remove last empty element

    masses := make([]int, len(lines))
    for i, line := range lines{
        masses[i], _ = strconv.Atoi(line)
    }

    return &masses
}



func main(){

    fmt.Println(fuel(12))
    fmt.Println(fuel(14))
    fmt.Println(fuel(1969))
    fmt.Println(fuel(100756))

    fname := "/home/drew/aedvent-code-2019/day 01/andreas - go/input.txt"
    masses := *readInput(fname)

    total_fuel := 0
    for _, mass := range masses {
        total_fuel += fuel(mass)
    }
    fmt.Println("part 1: ", total_fuel)
}
