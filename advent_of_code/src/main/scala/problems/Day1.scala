package problems

import util.FileUtils
import java.util.Map
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._
import util.Point
import scala.collection.immutable.Set

object Day1 {

  def main(args: Array[String]): Unit = {
    val input = FileUtils.readAllLines("/1.txt")
    part1(input)
    part2(input)
  }

  private def part1(input: List[String]) = {
    var instructions: Array[String] = input(0).replace(" ", "").split(',')

    var direction = 0
    var p: Point = new Point(0, 0)

    for (i <- instructions) {
      if (i(0).equals('R')) {
        direction = (direction + 1) % 4
      } else {
        direction = (direction + 3) % 4
      }
      var distance = i.substring(1).toInt
      direction match {
        case 0 => p.setY(p.getY + distance)
        case 1 => p.setX(p.getX + distance)
        case 2 => p.setY(p.getY - distance)
        case 3 => p.setX(p.getX - distance)
      }
    }

    println(math.abs(p.getX) + math.abs(p.getY))
  }

  private def part2(input: List[String]) = {
    var instructions: Array[String] = input(0).replace(" ", "").split(',')

    var direction = 0

    var xPos = 0
    var yPos = 0
    var history: ArrayBuffer[Point] = ArrayBuffer(new Point(xPos, yPos))

    breakable {
      for (i <- instructions) {
        if (i(0).equals('R')) {
          direction = (direction + 1) % 4
        } else {
          direction = (direction + 3) % 4
        }

        for (j <- 0 until i.substring(1).toInt) {
          direction match {
            case 0 => yPos += 1
            case 1 => xPos += 1
            case 2 => yPos -= 1
            case 3 => xPos -= 1
          }
          history += new Point(xPos, yPos)
          if (hasVisitedTwice(history)) {
            break
          }
        }

      }
    }

    println(math.abs(xPos) + math.abs(yPos))

  }

  private def hasVisitedTwice(history: ArrayBuffer[Point]): Boolean = {
    for (h <- history.reverse.tail) {
      if (h.equals(history.last)) {
        return true
      }
    }
    return false
  }

}
