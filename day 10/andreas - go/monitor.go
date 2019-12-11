package main

import (
    "fmt"
    "math"
    "sort"
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
    angle += math.Pi/2 //start pointing to top
    if angle < 0 { angle += 2*math.Pi } //no negativity
    //fmt.Println(monitor_x, monitor_y, "astroid:", astroid_x, astroid_y, "=>", angle)
    return angle
}
func distanceBetween(x,y int, ax,ay int) float64{
    astroid_x := float64(ax)
    astroid_y := float64(ay)
    monitor_x := float64(x)
    monitor_y := float64(y)

    dy := monitor_y-astroid_y
    dx := monitor_x-astroid_x
    return math.Sqrt(dy*dy + dx*dx)
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

func (g Galaxy) destroy() ([][2]int) {
    mx, my, _ := g.search()
    orderings := map[float64][][2]int{}

    //group by angle
    for _, astroid := range g.astroids{
        ax, ay := astroid[0], astroid[1]
        if ax == mx && ay == my { continue }
        angle := angleBetween(mx,my, ax,ay)

        if _, ex := orderings[angle]; !ex {
            orderings[angle] = [][2]int{}
        }

        orderings[angle] = append(orderings[angle], [2]int{ax,ay})
    }

    //sort each group by distance
    for _, astroids := range orderings {
        sort.Slice(astroids,
            func(i, j int) bool {
                dist_i := distanceBetween(mx, my, astroids[i][0], astroids[i][1])
                dist_j := distanceBetween(mx, my, astroids[j][0], astroids[j][1])
                return dist_i < dist_j
            },
        )
    }

    //get order
    count := 0
    order := make([][2]int, len(g.astroids)-1)
    for count < len(g.astroids)-1 {

        angles := make([]float64, len(orderings))
        i := 0; for key, _ := range orderings { angles[i] = key; i++ }
        sort.Float64s(angles)

        for _, angle := range angles {
            if len(orderings[angle]) == 0 {
                delete(orderings, angle)
            } else {
                destroyed := orderings[angle][0]
                fmt.Println("destroyed", destroyed, "=>", count, len(order))
                order[count] = destroyed
                count++
                orderings[angle] = orderings[angle][1:]
            }
        }

    }

    return order

}

func testPart1(){
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
}

func testPart2(){
    input := `.#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....#...###..
..#.#.....#....##`
    galaxy := parseInput(input)
    x,y,cnt := galaxy.search()
    fmt.Println(x,y,cnt)
    order := galaxy.destroy()
    fmt.Println(order)

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
    galaxy = parseInput(input)
    x,y,cnt = galaxy.search()
    fmt.Println(x,y,cnt)
    order = galaxy.destroy()
    for i, astroid := range order {
        fmt.Println(i+1,"=>", astroid)
    }
}


func main(){
    //part 1
    input := readInput("input.txt")
    galaxy := parseInput(input)
    x,y,cnt := galaxy.search()
    fmt.Println(x,y,cnt)

    order := galaxy.destroy()
    for i, astroid := range order {
        fmt.Println(i+1,"=>", astroid)
    }


}
