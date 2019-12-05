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

    for i, j := 0, 1 ; j < len(result); i, j = j, j+1 {
        if result[j] < result[i]{ return false }
    }

    duplicate := false
    for i := 0 ; i < len(result) && ! duplicate ; {

        cnt := 1
        j := i+1
        for j < len(result) && result[i] == result[j] {
            cnt++
            j++
        }

        if cnt == 2 { duplicate = true } //stop looping when duplicate is found
        i = j//move forward
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


    fmt.Println(isValid(112233))
    fmt.Println(isValid(123444))
    fmt.Println(isValid(111122))
    fmt.Println()

    fmt.Println(scanNumbers(168630, 718098))
}
