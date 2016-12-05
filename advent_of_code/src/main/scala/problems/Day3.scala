package problems

import util.FileUtils

object Day3 {

  def main(args: Array[String]): Unit = {
    val input = FileUtils.readAllLines("/3.txt")
    part1(input)
    part2(input)
  }

  private def part1(input: List[String]) = {
    var count: Int = 0

    for (line <- input) {
      var corners: Array[Int] = parseLine(line)

      if (isValidTriangle(corners)) {
        count += 1
      }
    }

    println(count)
  }

  private def part2(input: List[String]) = {
    var count: Int = 0
    var iter: Iterator[String] = input.toIterator

    while (iter.hasNext) {
      var l1: Array[Int] = parseLine(iter.next)
      var l2: Array[Int] = parseLine(iter.next)
      var l3: Array[Int] = parseLine(iter.next)

      for (i <- 0 to 2) {
        var corners: Array[Int] = Array(l1(i), l2(i), l3(i))
        if (isValidTriangle(corners)) {
          count += 1
        }
      }
    }

    println(count)
  }

  private def parseLine(line: String): Array[Int] = {
    line.trim().split("\\s+").map { x => x.toInt }
  }

  private def isValidTriangle(corners: Array[Int]): Boolean = {
    corners.filter { x => 2 * x >= corners.sum }.length == 0
  }
}
