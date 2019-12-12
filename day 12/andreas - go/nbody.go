package main


import (
    "fmt"
    "strconv"
    "strings"
    "io/ioutil"
)


func readInput(fname string) string{
    text, _ := ioutil.ReadFile(fname)
    return string(text)
}
func abs(val int) int{
    if val < 0 { return -val }
    return val
}


func parseInput(input string) []*Moon{
    lines := strings.Split(input, "\n")
    moons := make([]*Moon, 0, len(lines))

    for _, line := range lines {
        if len(line) == 0 { continue }
        line = line[1:len(line)-1]

        coords := make([]int, 3)
        for i, part := range strings.Split(line, ","){
            coords[i], _ = strconv.Atoi(strings.Split(part, "=")[1])
        }

        moons = append(moons, &Moon{
            coords[0], coords[1], coords[2],
            0,0,0,
        })
    }

    return moons
}



type Moon struct{
    x,y,z int
    dx, dy, dz int
}
func (m *Moon) move(){
    //update position
    m.x += m.dx
    m.y += m.dy
    m.z += m.dz
}
func (m Moon) totalEnergy() int{

    potential := abs(m.x) + abs(m.y) + abs(m.z)
    kinetic := abs(m.dx) + abs(m.dy) + abs(m.dz)

    return potential * kinetic
}


func run(input string, steps int) int{
    moons := parseInput(input)

    for step := 0; step < steps ; step++ {
        for i, moona := range moons {
            for j, moonb := range moons {
                if i == j { continue }
                if moona.x > moonb.x {
                    moona.dx--
                } else if moona.x < moonb.x {
                    moona.dx++
                }

                if moona.y > moonb.y {
                    moona.dy--
                } else if moona.y < moonb.y {
                    moona.dy++
                }

                if moona.z > moonb.z {
                    moona.dz--
                } else if moona.z < moonb.z {
                    moona.dz++
                }
            }
        }

        //fmt.Println(step)
        for _, moon := range moons {
            moon.move()
            //fmt.Println(moon)
        }
        //fmt.Println()
    }

    energy := 0
    for _, moon := range moons {
        energy += moon.totalEnergy()
    }
    return energy

}



func main(){

    input := `<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>`
    fmt.Println(run(input, 10))

    input = ` <x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>`
    fmt.Println(run(input, 100))


    input = readInput("input.txt")
    fmt.Println(run(input, 1000))

}
