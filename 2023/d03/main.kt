import java.io.File

val FileName = "input.txt"

class Map(fileName: String) {
  var v=mutableListOf<String>()
  var xSize = 0
  var ySize = 0

  init {
    File(fileName).forEachLine() {
      v.add(it)
      xSize = it.length
      ySize += 1
    }
  }

  fun value(x: Int, y: Int): Char = if (x in 0..xSize-1 && y in 0..ySize-1) v[y][x] else ' '

  fun neighs(x: Int, y: Int) : String {
    return (
      "${value(y-1, x-1)}" + value(y-1, x) + value(y-1, x+1)
      + value(y, x-1) + ' ' + value(y, x+1)
      + value(y+1, x-1) + value(y+1, x) + value(y+1, x+1))
  }
}

fun isDigit(c: Char) = c in '0'..'9'

fun hasPart(s: String) : Boolean {
  for (c in s) {
    if (c != '.' && c != ' ' && !isDigit(c)) return true
  }
  return false
}

class NumberPos(yy:Int, xx0: Int, xx1:Int, vvalue: Int) {
  val y = yy
  val x0 = xx0
  val x1 = xx1
  val value = vvalue
}

var numbers= mutableListOf<NumberPos>()

fun findNumbers(map: Map) {
  var result = 0
  for (y in 0..map.ySize-1) {
    var x = 0
    while (x < map.xSize) {
      val x0 = x
      var touchesPart = false
      while (x < map.xSize && isDigit(map.v[y][x])) {
        if (hasPart(map.neighs(y, x))) {
          touchesPart = true
        }
        x++
      }
      if (x0 < x) {
        val xV = map.v[y].substring(x0, x).toInt()
        // println(xV)
        if (touchesPart) {
          result += xV
          numbers.add(NumberPos(y, x0, x, xV))
        }
      }
      x++
    }
  }
  println(result)
}

fun gearRatios(map: Map) {
  var res = 0
  for (y in 0..map.ySize-1) {
    for (x in 0..map.xSize-1) {
      if (map.v[y][x] == '*') {
        var neighCount = 0
        var ratio = 1
        for (num in numbers) {
          if (num.y in y-1..y+1 && x in num.x0-1..num.x1) {
            neighCount+=1
            ratio *= num.value
          }
        }
        if (neighCount == 2) {
          res += ratio
        }
      }
    }
  }
  println(res)
}
// 528819
fun main()  {
  var map = Map(FileName)
  findNumbers(map)
  gearRatios(map)
}
