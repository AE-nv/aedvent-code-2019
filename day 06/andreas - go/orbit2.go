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
    parent *Planet
    children []*Planet
}

func (p Planet) print(){
    children := ""; for _, c := range p.children { children += c.name + " " }
    p_name := ""; if p.parent != nil { p_name = p.parent.name }
    fmt.Println(p.name, ":", p_name, "=>", children)
}

func parse(input string) map[string]*Planet {
    planets := map[string]*Planet{}
    lines := strings.Split(input, "\n")
    for i := 0 ; i < len(lines) ; i++ {
        line := lines[i]
        if len(line) == 0 { continue }

        parts := strings.Split(line, ")")
        if _, ok := planets[parts[0]]; !ok {
            planets[parts[0]] = &Planet{ parts[0], nil, []*Planet{} }
        }
        if _, ok := planets[parts[1]]; !ok {
            planets[parts[1]] = &Planet{ parts[1], nil, []*Planet{} }
        }

        planets[parts[1]].parent = planets[parts[0]]
        planets[parts[0]].children = append(planets[parts[0]].children, planets[parts[1]])
    }
    return planets
}

func findDistances(planets map[string]*Planet, start *Planet) map[string]int{
    distances := map[string]int{}

    dist := -1
    for x := start; x != nil; x = x.parent {
        distances[x.name] = dist
        dist++
    }

    return distances
}

func findPath(distances_you, distances_san map[string]int) int{
    //find node that connexts both
    shortest_dist := len(distances_you) + len(distances_san)

    for dest, dist := range distances_you {
        if _, ok := distances_san[dest]; ! ok { continue }

        total_distance := dist + distances_san[dest]
        if total_distance < shortest_dist {
            shortest_dist = total_distance
        }
    }

    return shortest_dist
}

func main(){
    //method
    //from you => travel to parent until you get at COM
    //from san => travel to parent until you get at COM
    //Both paths will share nodes => we can find the node X by minimizing
    //distance(you->X) + distance(san->x)
    //man in the middle attack :D

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
K)L
K)YOU
I)SAN`

    planets := parse(input)
    distances_you := findDistances(planets, planets["YOU"])
    fmt.Println("you")
    fmt.Println(distances_you)
    distances_san := findDistances(planets, planets["SAN"])
    fmt.Println("san")
    fmt.Println(distances_san)
    fmt.Println(findPath(distances_you, distances_san))

    input = readInput("input.txt")
    planets = parse(input)
    distances_you = findDistances(planets, planets["YOU"])
    fmt.Println("you")
    fmt.Println(distances_you)
    distances_san = findDistances(planets, planets["SAN"])
    fmt.Println("san")
    fmt.Println(distances_san)
    fmt.Println(findPath(distances_you, distances_san))
}
