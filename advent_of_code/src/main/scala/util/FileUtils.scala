package util

import scala.io.Source

object FileUtils {

  def readAllLines(fileName: String): List[String] = {
    Source.fromInputStream(getClass().getResourceAsStream(fileName)).getLines().toList
  }

}
