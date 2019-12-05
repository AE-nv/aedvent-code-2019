import java.io.File

fun main() {
    val prg =  Program2()
    println(prg.func("1,0,0,0,99"))
    println(prg.func("2,3,0,3,99"))
    println(prg.func("2,4,4,5,99,0"))
    println(prg.func("1,1,1,4,99,5,6,0,99"))
    println(prg.run())
}

class Program2 {
    fun run(): List<Int> {
        val lst = File("C:\\Users\\sstoelen\\vdabdev\\projects\\advent-seba\\src\\day1\\input2.txt").readText()
                .split(",")
                .map { it.toInt() }
                .toMutableList()
        lst[1] = 12
        lst[2] = 2
        return func(
                lst.joinToString(",")
        )
    }

    fun func(lst: String): List<Int> {
        val intlist = lst.split(',')
                .map { it.toInt() }
                .toMutableList()
        for (i in 0..intlist.size step 4)  {
            if (intlist[i] == 99) break
            else process(intlist, i)
        }
        return intlist
    }

    fun process (intlist: MutableList<Int>, index: Int) {
        when (intlist[index]) {
            1 -> {
                val first = intlist[intlist[index+1]]
                val second = intlist[intlist[index+2]]
                intlist[intlist[index+3]] = first + second
            }
            2 -> {
                val first = intlist[intlist[index+1]]
                val second = intlist[intlist[index+2]]
                intlist[intlist[index+3]] = first * second
            }
        }
    }
}