package problems

import util.FileUtils

object Day8 {

  def main(args: Array[String]): Unit = {
    val input = FileUtils.readAllLines("/8.txt")
    part1(input)
    part2(input)
  }

  def part1(input: List[String]) = {
    var screen: Array[Array[Boolean]] = applyInputToScreen(input)
    println(screen.flatten.filter(_ == true).length)
  }

  def part2(input: List[String]) = {
    var screen: Array[Array[Boolean]] = applyInputToScreen(input)
    for (i <- 0 until screen(0).length) {
      for (j <- 0 until screen.length) {
        print(if (screen(j)(i)) "#" else " ")
      }
      println
    }
  }

  private def applyInputToScreen(input: List[String]): Array[Array[Boolean]] = {
    var screen: Array[Array[Boolean]] = Array.ofDim[Boolean](50, 6)

    for (line <- input) {
      if (line.startsWith("rect")) {
        var rectSize: Array[Int] = line.substring(5).split('x').map(_.toInt)
        rect(screen, rectSize(0), rectSize(1))
      } else if (line.startsWith("rotate row")) {
        var rotateDetails: Array[Int] = line.substring(13).split(" by ").map(_.toInt)
        rotateRow(screen, rotateDetails(0), rotateDetails(1))
      } else if (line.startsWith("rotate column")) {
        var rotateDetails: Array[Int] = line.substring(16).split(" by ").map(_.toInt)
        rotateColumn(screen, rotateDetails(0), rotateDetails(1))
      }
    }

    return screen
  }

  private def rect(screen: Array[Array[Boolean]], x: Int, y: Int) = {
    for (i <- 0 until x) {
      for (j <- 0 until y) {
        screen(i)(j) = true
      }
    }
  }

  private def rotateRow(screen: Array[Array[Boolean]], y: Int, amount: Int) = {
    var originalRow: Array[Boolean] = screen.map(_(y))

    for (i <- 0 until screen.length) {
      screen(i)(y) = originalRow((screen.length + i - amount) % screen.length)
    }
  }

  private def rotateColumn(screen: Array[Array[Boolean]], x: Int, amount: Int) = {
    var originalColumn: Array[Boolean] = screen(x).clone()

    for (i <- 0 until screen(0).length) {
      screen(x)(i) = originalColumn((screen(0).length + i - amount) % screen(0).length)
    }
  }

}
