import java.io.File
import kotlin.math.*

val colorMap = mapOf("red" to 0, "green" to 1, "blue" to 2)

class Game(line: String) {
  val gameId: Int
  val possible: Boolean
  val power: Int
  init {
    val l = line.split(":")
    gameId = l[0].split(" ")[1].toInt()
    var colors = mutableListOf(0, 0, 0)
    l[1].split(";").map{
      for (x in it.split(",")) {
        val x2 = x.trim().split(" ")
        val color = x2[1]
        val num = x2[0].toInt()
        val id: Int = colorMap[color]!!
        colors[id] = max(colors[id], num)
      }
    }
    possible = colors[0] <= 12 && colors[1] <= 13 && colors[2] <= 14
    power = colors[0] * colors[1] * colors[2]
  }
}

fun main() {
  var res1 = 0
  var res2 = 0
  File("input.txt").forEachLine() {
    val g = Game(it)
    if (g.possible) {
      res1 += g.gameId
    }
    res2 += g.power
  }
  println(res1)
  println(res2)
}
