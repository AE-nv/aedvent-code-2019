package day4

fun main() {
//    println(PasswordValidator().meetsCriteria("123444"))
    println(PasswordGenerator().getNumberOfValidPasswordsInRange(145852,616942))
}

class PasswordGenerator {

    fun getNumberOfValidPasswordsInRange(start: Int, end: Int): Int {
        val validator = PasswordValidator()
        var count = 0
        for(currentPassword in start..end) {
            if (validator.meetsCriteria(currentPassword.toString())) count += 1
        }
        return count
    }
}

class PasswordValidator {

    fun meetsCriteria(password : String) : Boolean {
        if (password.length != 6) return false
        if (! hasMatchingGroupOfExactlyTwo(password)) return false
        if (! neverDecreases(password)) return false

        return true
    }

    private fun hasMatchingGroupOfExactlyTwo(password: String): Boolean {
        val matchingGroups = mutableListOf<Pair<Char, Int>>()
        var currentIndex = 0
        while (currentIndex < password.length) {
            val matchingGroup = findMatchingGroupSize(currentIndex, password)
            matchingGroups.add(matchingGroup)
            currentIndex += matchingGroup.second
        }

        matchingGroups.forEach{
            if(it.second == 2) return true
        }

        return false
    }

    private fun findMatchingGroupSize(currentIndex: Int, password: String): Pair<Char, Int> {
        var matching = true
        var matchSize = 1
        var index = currentIndex + 1
        val currentNumber = password[currentIndex]
        while(matching && index < password.length) {
            if(password[index] == currentNumber) {
                matchSize += 1
                index += 1
            } else {
                matching = false
            }
        }
        return Pair(currentNumber, matchSize)
    }

    private fun hasEqualAdjacentNumbers(password: String): Boolean {
        for(index in 1 until password.length) {
            if (password[index-1] == password[index]) return true
        }
        return false
    }

    private fun neverDecreases(password: String): Boolean {
        for(index in 1 until password.length) {
            if (password[index-1] > password[index]) return false
        }
        return true
    }
}