package main

import (
    "fmt"
    "strings"
    "io/ioutil"
)


func readInput(fname string) string{
    text, _ := ioutil.ReadFile(fname)
    return string(text)
}

type Planet struct {
    name string
    direct_orbit *Planet
}

func parse(input string) map[string]*Planet {
    planets := map[string]*Planet{}
    lines := strings.Split(input, "\n")
    for i := 0 ; i < len(lines) ; i++ {
        line := lines[i]
        if len(line) == 0 { continue }

        parts := strings.Split(line, ")")
        if _, ok := planets[parts[0]]; !ok {
            planets[parts[0]] = &Planet{ parts[0], nil }
        }
        if _, ok := planets[parts[1]]; !ok {
            planets[parts[1]] = &Planet{ parts[1], nil }
        }

        planets[parts[1]].direct_orbit = planets[parts[0]]
    }
    return planets
}

func countOrbits(planet *Planet) int {
    count := -1
    for plnt := planet ; plnt != nil; plnt = plnt.direct_orbit {
        count++
    }
    return count
}

func main(){

    input := `COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L`

    total := 0
    planets := parse(input)
    for name, planet := range planets {
        count := countOrbits(planet)
        total += count
        fmt.Println(
            name, count,
        )
    }
    fmt.Println(total)


    total = 0
    input = readInput("input.txt")
    planets = parse(input)
    for name, planet := range planets {
        count := countOrbits(planet)
        total += count
        fmt.Println(
            name, count,
        )
    }
    fmt.Println(total)
}
