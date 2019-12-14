package main

import (
    "fmt"
)


func createBoard() Board{
    return Board { map[int]map[int]int{} }
}
type Board struct {
    data map[int]map[int]int
}
func (b Board) getValue(x,y int) int {
    if _, ex := b.data[y] ; ! ex { return 0 }

    val, ex := b.data[y][x]
    if ex { return val }
    return 0
}
func (b *Board) setValue(x,y, val int){
    if _, ex := b.data[y]; !ex {
        b.data[y] = map[int]int{}
    }
    b.data[y][x] = val
}
func (b Board) Print(symbols []string) {
    miny, maxy := 0, 0
    minx, maxx := 0, 0

    for y, row := range b.data {
        if y < miny { miny = y }
        if y > maxy { maxy = y }

        for x, _ := range row {
            if x < minx { minx = x }
            if x > maxx { maxx = x }
        }
    }

    for y := miny ; y <= maxy ; y++ {
        for x := minx ; x <= maxx ; x++ {
            value := b.getValue(x,y)
            fmt.Print(symbols[value])
        }
        fmt.Println()
    }
}
