package problems

import util.FileUtils

object Day9 {

  def main(args: Array[String]): Unit = {
    val input = FileUtils.readAllLines("/9.txt")
    part1(input)
    part2(input)
  }

  def part1(input: List[String]) = {
    var line: String = input(0)

    var count = 0
    var i = 0

    while (i < line.length()) {
      if (line(i) == '(') {
        var markerEnd = line.indexWhere(_ == ')', i)
        // For (6x9), marker(0) = 6, marker(1) = 9
        var marker: Array[Int] = line.substring(i + 1, markerEnd).split('x').map(_.toInt)
        count += marker(0) * marker(1)
        i = markerEnd + marker(0)
      } else {
        count += 1
      }
      i += 1
    }
    println(count)
  }

  def part2(input: List[String]) = {
    var line: String = input(0)
    var occurrences: Array[Int] = Array.fill[Int](line.length())(1)

    var count: Long = 0
    var i = 0

    /* example input and how the occurrences array changes
     * X(8x2)(3x3)ABCY -> XABCABCABCABCABCABCY
     * 111111222222221 (after 8x2 is read)
     * 111111222226661 (after 3x3 is read)
     * 111111111116661 (at the end, now just add up all the occurrences of A-Z)
     * 1          6661 = 20
     */

    while (i < line.length()) {
      if (line(i) == '(') {
        var markerEnd = line.indexWhere(_ == ')', i)
        var marker: Array[Int] = line.substring(i + 1, markerEnd).split('x').map(_.toInt)
        for (j <- markerEnd + 1 until markerEnd + 1 + marker(0)) {
          occurrences(j) *= marker(1)
        }
        i = markerEnd
      }
      i += 1
    }

    for (i <- 0 until line.length()) {
      if (('A' to 'Z') contains line(i)) {
        count += occurrences(i)
      }
    }
    println(count)
  }

}
