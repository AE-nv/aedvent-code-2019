package main


import (
    "fmt"
    "strconv"
    "io/ioutil"
)

func readInput(fname string) string{
    input, _ := ioutil.ReadFile(fname)
    return string(input)
}

func getLayers(input string, wide, tall int) [][][]int {

    layer_size := wide * tall
    layer_count := len(input) / layer_size
    layers := make([][][]int, layer_count)

    for i, _ := range layers {
        layer := make([][]int, tall)
        for y := 0; y < tall; y++ {
            line := make([]int, wide)
            for x := 0; x < wide; x++{

                char := string(input[layer_size*i + y*wide + x])
                line[x], _ = strconv.Atoi(char)
            }
            layer[y] = line
        }
        layers[i] = layer
    }

    return layers
}

func findLayer(layers [][][]int) int{

    min_count := -1
    min_layer := -1

    for i, layer := range layers{
        count := 0
        for _, row := range layer {
            for _, digit := range row {
               if digit == 0 { count++ }
            }
        }

        if min_count < 0 || count < min_count {
            min_count = count
            min_layer = i
        }
    }

    return min_layer
}

func checksum(layer [][]int) int{
    ones, twos := 0, 0
    for _, row := range layer {
        for _, digit := range row {
            if digit == 1 {
                ones++
            } else if digit == 2 {
                twos++
            }
        }
    }
    return ones * twos
}

func render(layers [][][]int) [][]int{
    if len(layers) <= 1 { return layers[0] }

    tall := len(layers[0])
    wide := len(layers[0][0])

    image := make([][]int, tall)
    for y := 0; y < tall ; y++ {
        row := make([]int, wide)
        for x := 0; x < wide; x++ {
            row[x] = getValue(layers, y,x)
        }
        image[y] = row
    }

    return image
}

func getValue(layers [][][]int, y, x int) int{

    values := make([]int, len(layers))
    for i, layer := range layers {
        values[i] = layer[y][x]
    }

    for _, value := range values {
        if value == 0 {
            return 0
        } else if value == 1 {
            return 1
        }
    }

    return -1
}


func main(){
    fmt.Println(getLayers("123456789012", 3, 2))
    fmt.Println(render(getLayers("0222112222120000", 2,2)))

    input := readInput("input.txt")
    layers := getLayers(input, 25,6)
    min_layer := findLayer(layers)
    checksum := checksum(layers[min_layer])
    fmt.Println(checksum)
    image := render(layers)
    fmt.Println()
    for _, row := range image {
        fmt.Println(row)
    }

}
