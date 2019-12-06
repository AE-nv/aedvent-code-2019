package day2

import java.io.File

fun main() {
    var result = listOf<Int>()
    for (noun in 0..99) {
        for(verb in 0..99) {
            val input = Loader().loadProgramInput()
            result = Runner().run(noun, verb, input)
            println(result)
            if (result[0] == 19690720) break
        }
        if (result[0] == 19690720) break
    }
    println("GOT RESULT")
    println(result)
}

class Calculator {

    fun processInstructions(instructionList: MutableList<Int>): List<Int> {
        for (currentIndex in 0..instructionList.size step 4)  {
            if (instructionList[currentIndex] == 99) break
            else processInstruction(instructionList, currentIndex)
        }
        return instructionList
    }

    fun processInstruction (instructionList: MutableList<Int>, index: Int) {
        when (instructionList[index]) {
            1 -> {
                val first = instructionList[instructionList[index+1]]
                val second = instructionList[instructionList[index+2]]
                instructionList[instructionList[index+3]] = first + second
            }
            2 -> {
                val first = instructionList[instructionList[index+1]]
                val second = instructionList[instructionList[index+2]]
                instructionList[instructionList[index+3]] = first * second
            }
        }
    }
}

class Runner {
    fun run(noun : Int, verb : Int, freshProgram : MutableList<Int>) : List<Int> {
        freshProgram[1] = noun
        freshProgram[2] = verb
        val calculator : Calculator = Calculator()
        val result = calculator.processInstructions(freshProgram)
        return listOf(result[0], noun, verb)
    }
}

class Loader {
    fun loadProgramInput() : MutableList<Int> {
        return File("src/day2/input.txt").readText()
            .split(",")
            .map { it.toInt() }
            .toMutableList()
    }
}