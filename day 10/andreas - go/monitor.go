package main

import (
    "fmt"
    "math"
    "strings"
    "io/ioutil"
)

func readInput(fname string) string{
    text, _ := ioutil.ReadFile(fname)
    return string(text)
}

func angleBetween(x,y int, ax,ay int) float64 {
    astroid_x := float64(ax)
    astroid_y := float64(ay)
    monitor_x := float64(x)
    monitor_y := float64(y)
    angle := math.Atan2( (astroid_y-monitor_y), (astroid_x-monitor_x) )
    //fmt.Println(monitor_x, monitor_y, "astroid:", astroid_x, astroid_y, "=>", angle)
    return angle
}

type Galaxy struct {
    positions [][]bool
    astroids [][2]int
    rows int
    columns int
}

func parseInput(input string) Galaxy {
    lines := strings.Split(input, "\n")
    rows := len(lines)
    columns := len(lines[0])

    astroids := [][2]int{}
    positions := make([][]bool, len(lines))
    for y, line := range lines {
        row := make([]bool, len(line))
        for x := 0; x < len(line) ; x++ {
            row[x] = line[x] == '#'
            if line[x] == '#' {
                astroids = append(astroids, [2]int{x,y})
            }
        }
        positions[y] = row
    }
    return Galaxy{ positions, astroids, rows, columns }
}

func (g Galaxy) astroidsInSight(x,y int) int{
    angleCount := map[float64]bool{}
    for _, astroid := range g.astroids {
        ax, ay := astroid[0], astroid[1]
        if ax == x && ay == y { continue }
        angleCount[ angleBetween(x,y,ax,ay) ] = true
    }
    return len(angleCount)
}

func (g Galaxy) search() (int, int, int){
    best_count := 0
    best_x, best_y := 0, 0
    for _, position := range g.astroids {
        x, y := position[0], position[1]
        count := g.astroidsInSight(x,y)
        //fmt.Println(x,y,count)
        if count > best_count{
            best_count = count
            best_y = y
            best_x = x
        }
    }
    return best_x, best_y, best_count
}


func main(){

    input := `.#..#
.....
#####
....#
...##`

    x,y,cnt := parseInput(input).search()
    fmt.Println(x,y,cnt)

    input = `......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####`
    x,y,cnt = parseInput(input).search()
    fmt.Println(x,y,cnt)

    input = `#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.`
    x,y,cnt = parseInput(input).search()
    fmt.Println(x,y,cnt)


    input = `.#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..`
    x,y,cnt = parseInput(input).search()
    fmt.Println(x,y,cnt)

    input = `.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##`
    x,y,cnt = parseInput(input).search()
    fmt.Println(x,y,cnt)

    input = readInput("input.txt")
    x,y,cnt = parseInput(input).search()
    fmt.Println(x,y,cnt)
}
