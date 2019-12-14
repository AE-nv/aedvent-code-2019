package main


import(
    "fmt"
    "io/ioutil"
)

func readInput(fname string) string{
    text, _ := ioutil.ReadFile(fname)
    return string(text)
}

func getValues(program Program) []int{
    values := []int{}

    for value, done := program.run(); ! done; value, done = program.run(){
        values = append(values, value)
    }

    return values
}

func handleOutput(values []int) Board{
    board := createBoard()

    for i := 0; i < len(values) ; i+=3{
        x := values[i]
        y := values[i+1]
        tile_id := values[i+2]
        board.setValue(x,y,tile_id)
    }

    return board
}
func count(b Board) int {
    count := 0
    for y, row := range b.data {
        for x, _ := range row {
            if val, ex := b.data[y][x]; ex && val == 2 { count++ }
        }
    }
    return count
}


func main(){
    /*
    values := []int{1,2,3,6,5,4}
    board := handleOutput(values)
    board.Print()
    fmt.Println(board.count())
    */

    input := readInput("./input.txt")
    program := createProgram(input)
    values := getValues(program)
    board := handleOutput(values)
    board.Print()
    fmt.Println(count(board))
}
