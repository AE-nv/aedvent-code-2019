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
    ones := 0
    twos := 0
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


func main(){
    fmt.Println(getLayers("123456789012", 3, 2))

    input := readInput("input.txt")
    layers := getLayers(input, 25,6)
    min_layer := findLayer(layers)
    checksum := checksum(layers[min_layer])
    fmt.Println(checksum)

}
