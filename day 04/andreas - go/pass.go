package main


import (
    "fmt"
)

func isValid(number int) bool {

    result := make([]int, 6)
    for i := 0; number > 0 ; i++ {
        result[5-i] = number % 10
        number /= 10
    }

    duplicate := false
    for i, j := 0, 1 ; j < len(result); i, j = j, j+1 {

        if result[i] == result[j]{ duplicate = true }
        if result[j] < result[i]{ return false }
    }
    return duplicate
}


func scanNumbers(start, stop int) int {

    count:=0
    for number := start; number < stop; number++ {
        if isValid(number){ count++ }
    }
    return count
}


func main(){


    fmt.Println(isValid(111111))
    fmt.Println(isValid(223450))
    fmt.Println(isValid(123789))
    fmt.Println(isValid(168999))
    fmt.Println()

    fmt.Println(scanNumbers(168630, 718098))
}
