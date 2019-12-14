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

func part1(){
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
    board.Print([]string{ " ", "|", "#", "-", "o", })
    fmt.Println(count(board))
}




func main(){
    //part1()

    program := createProgram(readInput("./input.txt"))
    program.data[0] = 2
    board := createBoard()

    x, done, input := program.runInput()
    y, done, input := program.runInput()
    tile_id, done, input := program.runInput()
    bal_x, bal_y := 0, 0
    paddle_x, paddle_y := 0, 0

    for ! done {

        if x == -1 && y == 0 {
            fmt.Println("score", tile_id)
        } else if input {
            board.Print([]string{ " ", "|", "#", "-", "o", })
            fmt.Println("input", "bal:", bal_x, bal_y, "paddle:", paddle_x, paddle_y)

            if paddle_x < bal_x {
                program.setInput(1)
            } else if paddle_x > bal_x {
                program.setInput(-1)
            } else {
                program.setInput(0)
            }
        } else if tile_id == 3 {
            paddle_x, paddle_y = x , y
            board.setValue(x,y,tile_id)
        } else if tile_id == 4 {
            bal_x, bal_y = x , y
            board.setValue(x,y,tile_id)
        } else {
            board.setValue(x,y,tile_id)
        }


        x, done, input = program.runInput()
        y, done, input = program.runInput()
        tile_id, done, input = program.runInput()
    }
}
