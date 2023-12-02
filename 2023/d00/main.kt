import java.io.File

fun main() {
    // TODO: do we need BufferedReader?
    File("input.txt").bufferedReader().forEachLine { println("Hello, " + it + "!") }
}
