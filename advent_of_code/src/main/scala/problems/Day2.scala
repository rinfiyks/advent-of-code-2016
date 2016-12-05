package problems

import util.FileUtils

object Day2 {

  def main(args: Array[String]): Unit = {
    val input = FileUtils.readAllLines("/2.txt")
    part1(input)
    part2(input)
  }

  private def part1(input: List[String]) = {
    var xPos = 1
    var yPos = 1

    for (line <- input) {
      for (c <- line) {
        c match {
          case 'U' => yPos = math.min(2, yPos + 1)
          case 'R' => xPos = math.min(2, xPos + 1)
          case 'D' => yPos = math.max(0, yPos - 1)
          case 'L' => xPos = math.max(0, xPos - 1)
        }
      }
      var number = (xPos + 1) + (2 - yPos) * 3
      print(number)
    }
    println
  }

  private def part2(input: List[String]) = {
    var keypad: Array[Array[Char]] = Array(
      Array('0', '0', '1', '0', '0'),
      Array('0', '2', '3', '4', '0'),
      Array('5', '6', '7', '8', '9'),
      Array('0', 'A', 'B', 'C', '0'),
      Array('0', '0', 'D', '0', '0'))

    var xPos: Int = 1
    var yPos: Int = 1

    for (line <- input) {
      for (c <- line) {
        var xPosNew: Int = xPos
        var yPosNew: Int = yPos
        c match {
          case 'U' => yPosNew = math.max(0, yPos - 1)
          case 'R' => xPosNew = math.min(4, xPos + 1)
          case 'D' => yPosNew = math.min(4, yPos + 1)
          case 'L' => xPosNew = math.max(0, xPos - 1)
        }
        if (keypad(yPosNew)(xPosNew) != '0') {
          xPos = xPosNew
          yPos = yPosNew
        }
      }

      print(keypad(yPos)(xPos))
    }
    println
  }

}
