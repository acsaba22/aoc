import java.io.File

val file = "input.txt"

fun lineNumber(s: String): Int {
  var first = -1
  var last = -1
  for (c in s) {
    if ('0' <= c && c <= '9') {
      val v = c - '0'
      last = v
      if (first < 0) {
        first = v
      }
    }
  }
  return first*10+last
}

fun part1() {
  var result = 0
  File(file).bufferedReader().forEachLine {
    result += lineNumber(it)
  }
  println(result)
}

fun lineNamedNumber(s: String): Int {
  var s2 = s.replace("one", "one1one")
  s2 = s2.replace("two", "two2two")
  s2 = s2.replace("three", "three3three")
  s2 = s2.replace("four", "four4four")
  s2 = s2.replace("five", "five5five")
  s2 = s2.replace("six", "six6six")
  s2 = s2.replace("seven", "seven7seven")
  s2 = s2.replace("eight", "eight8eight")
  s2 = s2.replace("nine", "nine9nine")
  return(lineNumber(s2))
}


fun part2() {
  var result = 0
  File(file).bufferedReader().forEachLine {
    result += lineNamedNumber(it)
  }
  println(result)

}

fun main() {
  part1()
  part2()
}
