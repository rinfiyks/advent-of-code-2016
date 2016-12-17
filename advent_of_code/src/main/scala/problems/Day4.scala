package problems

import util.FileUtils

object Day4 {

  def main(args: Array[String]): Unit = {
    val input = FileUtils.readAllLines("/4.txt")
    part1(input)
    part2(input)
  }

  def part1(input: List[String]) = {
    var sectorSum: Int = 0
    for (line <- input) {
      var name: String = line.substring(0, line.length() - 11).replaceAll("-", "")
      var sector: Int = line.substring(line.length() - 10, line.length() - 7).toInt
      var checksum: String = line.substring(line.length() - 6, line.length() - 1)
      if (getChecksum(name).equals(checksum)) {
        sectorSum += sector
      }
    }

    println(sectorSum)
  }

  def part2(input: List[String]) = {
    for (line <- input) {
      var name: String = line.substring(0, line.length() - 11)
      var sector: Int = line.substring(line.length() - 10, line.length() - 7).toInt
      var checksum: String = line.substring(line.length() - 6, line.length() - 1)
      if (getChecksum(name.replaceAll("-", "")).equals(checksum)) {
        var realName: String = caesarShift(name, sector)
        if (realName.contains("north")) {
          println(realName + " " + sector)
        }
      }
    }
  }

  private def getChecksum(name: String): String = {
    var charCounts: collection.mutable.Map[Char, Int] = collection.mutable.Map.empty

    for (c <- name) {
      if (charCounts.contains(c)) {
        charCounts.update(c, charCounts(c) + 1)
      } else {
        charCounts += c -> 1
      }
    }

    return charCounts.toSeq.sortBy(r => (-r._2, r._1)).take(5).map(_._1).mkString
  }

  private def caesarShift(input: String, shiftAmount: Int): String = {
    val alphabet: String = "abcdefghijklmnopqrstuvwxyz"
    return input.map({
      case '-' => ' '
      case c => alphabet((alphabet.indexOf(c) + shiftAmount) % alphabet.length())
    })
  }
}
