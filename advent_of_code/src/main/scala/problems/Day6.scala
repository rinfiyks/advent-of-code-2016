package problems

import util.FileUtils

object Day6 {

  def main(args: Array[String]): Unit = {
    val input = FileUtils.readAllLines("/6.txt")
    part1(input)
    part2(input)
  }

  def part1(input: List[String]) = {
    for (column <- input.transpose) {
      print(column.groupBy(identity).maxBy(_._2.size)._1)
    }
    println
  }

  def part2(input: List[String]) = {
    for (column <- input.transpose) {
      print(column.groupBy(identity).minBy(_._2.size)._1)
    }
    println
  }

}
