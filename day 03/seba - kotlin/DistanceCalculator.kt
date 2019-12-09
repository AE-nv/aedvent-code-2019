package day3

import java.io.File
import kotlin.math.abs

fun main() {
    val wires = File("src/day3/input.txt").readLines().map { it.split(',') }

//    val distance = ManhattanDistanceCalculator().calculateDistanceForWires(wires[0], wires[1])
    val distance = StepsDistanceCalculator(Grid()).calculateDistanceForWires(wires[0], wires[1])
    println(distance)

}

class ManhattanDistanceCalculator {

    fun calculateDistanceForWires(wire1 : List<String>, wire2: List<String>): Int {
        val grid = Grid()
        wire1.forEach { grid.addLine(1, it) }
        wire2.forEach { grid.addLine(2, it) }

        val crossings = grid.getCrossings()
        return getDistanceOfClosesCrossing(crossings)

    }

    private fun getDistanceOfClosesCrossing(crossings: Collection<Position>): Int {
        return crossings.map { position -> getMHDistanceToCenter(position) }.min() ?: 0

    }

    private fun getMHDistanceToCenter(position: Position): Int {
        return abs(position.x) + abs(position.y)
    }

}

class StepsDistanceCalculator(private val grid: Grid) {

    fun calculateDistanceForWires(wire1 : List<String>, wire2: List<String>): Int {
        wire1.forEach { grid.addLine(1, it) }
        wire2.forEach { grid.addLine(2, it) }

        val crossings = grid.getCrossings()
        return getDistanceOfClosesCrossing(crossings)
    }
    private fun getDistanceOfClosesCrossing(crossings: Collection<Position>): Int {
        return crossings.map { position -> getStepsDistance(position) }.min() ?: 0

    }

    private fun getStepsDistance(position: Position): Int {
        val wire1Steps = grid.wireSteps[1]!![position]!!
        val wire2Steps = grid.wireSteps[2]!![position]!!
        return wire1Steps + wire2Steps
    }


}

class Grid {
    val map = mutableMapOf<Position, MutableCollection<Int>>()
    val currentPositions = mutableMapOf(1 to Position(0,0), 2 to Position(0,0))
    val currentSteps = mutableMapOf(1 to 0, 2 to 0)
    val wireSteps = mutableMapOf<Int, MutableMap<Position, Int>>(1 to mutableMapOf(Position(0,0) to 0),2 to mutableMapOf(Position(0,0) to 0))

    fun addLine(wire : Int, line : String) {
        val direction = line[0]
        val amount = line.substring(1).toInt()

        when(direction) {
         'R' -> goRight(wire, amount)
         'L' -> goLeft(wire, amount)
         'U' -> goUp(wire, amount)
         'D' -> goDown(wire, amount)
        }

    }

    fun getCrossings(): Collection<Position> {
        return map.keys
            .filter { pos -> map[pos]!!.size > 1 }
            .filter { pos -> pos != Position(0,0) }
    }

    private fun goRight(wire: Int, amount: Int) {
        val y = currentPositions[wire]!!.y
        val startX = currentPositions[wire]!!.x + 1
        val endX = currentPositions[wire]!!.x + amount
        for(x in startX..endX step 1) {
            addWire(wire, Position(x, y))
        }
    }

    private fun goLeft(wire: Int, amount: Int) {
        val y = currentPositions[wire]!!.y
        val startX = currentPositions[wire]!!.x - 1
        val endX = currentPositions[wire]!!.x - amount
        for(x in startX downTo endX step 1) {
            addWire(wire, Position(x, y))
        }
    }

    private fun goUp(wire: Int, amount: Int) {
        val x = currentPositions[wire]!!.x
        val startY = currentPositions[wire]!!.y + 1
        val endY = currentPositions[wire]!!.y + amount
        for(y in startY..endY step 1) {
            addWire(wire, Position(x, y))
        }
    }

    private fun goDown(wire: Int, amount: Int) {
        val x = currentPositions[wire]!!.x
        val startY = currentPositions[wire]!!.y - 1
        val endY = currentPositions[wire]!!.y - amount
        for(y in startY downTo endY step 1) {
            addWire(wire, Position(x, y))
        }
    }

    private fun addWire(wire: Int, position: Position) {
        if (map[position] == null) {
            map[position] = mutableSetOf(wire)
        } else {
            map[position]!!.add(wire)
        }
        currentPositions[wire] = position
        currentSteps[wire] = currentSteps[wire]!! + 1
        wireSteps[wire]!![position] = currentSteps[wire]!!
    }
}

data class Position(val x : Int, val y : Int)