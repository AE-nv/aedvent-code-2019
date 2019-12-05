package main

import (
	"fmt"
    "io/ioutil"
    "strings"
    "strconv"
)

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


func fuelBasic(mass int) int{
	return (mass/3)-2
}

func fuelDelux(mass int) int{
    fuel := fuelBasic(mass)
    mass = fuel
    total_fuel := 0
    for  ; fuel > 0 ; {
        total_fuel += fuel
        mass = fuel
        fuel = fuelBasic(mass)
    }

    return total_fuel
}


func main(){

    //part 1
    fmt.Println(fuelBasic(12))
    fmt.Println(fuelBasic(14))
    fmt.Println(fuelBasic(1969))
    fmt.Println(fuelBasic(100756))

    fname := "/home/drew/aedvent-code-2019/day 01/andreas - go/input.txt"
    masses := *readInput(fname)

    basic_fuel := 0
    for _, mass := range masses {
        basic_fuel += fuelBasic(mass)
    }
    fmt.Println("part 1: ", basic_fuel)

    //part 2
    fmt.Println(fuelDelux(12))
    fmt.Println(fuelDelux(14))
    fmt.Println(fuelDelux(1969))
    fmt.Println(fuelDelux(100756))

    delux_fuel := 0
    for _, mass := range masses {
        delux_fuel += fuelDelux(mass)
    }
    fmt.Println("part 2: ", delux_fuel)

}
