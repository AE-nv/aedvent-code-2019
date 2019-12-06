package day6

import java.io.File

fun main() {
    val input = File("src/day6/input.txt").readLines()
    println(OrbitMap().getDistanceToSanta(input))
}

class OrbitMap {

    val orbitmap: MutableMap<String, MutableSet<String>> = mutableMapOf()

    fun getTotalNumberOfOrbits(inputList: List<String>) : Int {
        buildOrbitMap(inputList)
        println(orbitmap)
        var totalOrbits = 0
        orbitmap.keys.forEach { totalOrbits += calculateAllOrbits(it) }
        return totalOrbits
    }

    fun getDistanceToSanta(inputList: List<String>) : Int {
        buildOrbitMap(inputList)
        val santasAncestors = getAncestorsOf("SAN", mutableListOf())
        val yourAncestors = getAncestorsOf("YOU", mutableListOf())
        val lowestCommonAncestor = findLowestCommonAncestor(yourAncestors, santasAncestors)
        return santasAncestors.indexOf(lowestCommonAncestor) + yourAncestors.indexOf(lowestCommonAncestor)
    }

    private fun findLowestCommonAncestor (
        yourAncestors: MutableList<String>,
        santasAncestors: MutableList<String>
    ): String {
        yourAncestors.forEach{
            if (santasAncestors.contains(it)) return it
        }
        return ""
    }

    private fun getAncestorsOf(s: String, ancestors: MutableList<String>) : MutableList<String> {
        ancestors.add(s)
        return if (getParentOf(s) == "") {
            ancestors.removeAt(0)
            ancestors
        } else {
            getAncestorsOf(getParentOf(s), ancestors)
        }
    }

    private fun getParentOf(target: String): String {
        orbitmap.keys.forEach {
            if(orbitmap[it]!!.contains(target)) return it
        }
        return ""
    }

    private fun calculateAllOrbits(planetName: String) : Int {
        return if (orbitmap[planetName] == null) {
            0
        } else {
            var orbits = orbitmap[planetName]!!.size
            orbitmap[planetName]!!.forEach {
                orbits += calculateAllOrbits(it)
            }
            orbits
        }


    }

    private fun buildOrbitMap(inputList: List<String>) {
        inputList.forEach{
            val splitString = it.split(')')
            val centerPlanet = splitString[0]
            val planetThatOrbits = splitString[1]
            val setOfPlanetsThatOrbits = orbitmap[centerPlanet]
            if (setOfPlanetsThatOrbits == null) {
                val newSetOfPlanetsThatOrbit = mutableSetOf(planetThatOrbits)
                orbitmap[centerPlanet] = newSetOfPlanetsThatOrbit
            } else {
                setOfPlanetsThatOrbits.add(planetThatOrbits)
            }
        }
    }
}
