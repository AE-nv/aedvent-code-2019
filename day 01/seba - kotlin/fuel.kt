fun main() {
    val prg =  Program()
    println(prg.caclulateFuelNeeds())
    //println(prg.getFuelForMass(14))
}

class Program {
    fun caclulateFuelNeeds() : Int {
        return File("C:\\Users\\sstoelen\\vdabdev\\projects\\advent-seba\\src\\day1\\input.txt").readLines()
                .map { it.toInt() }
                .map { getFuelForMass(it) }
                .sum()
    }

    fun getFuelForMass(mass: Int) : Int {
        var fuel = (mass / 3) - 2
        if (fuel > 0) {
            fuel += getFuelForMass(fuel)
        }
        return if (fuel > 0) fuel else 0
    }


}