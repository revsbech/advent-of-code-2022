package utils

import scala.io.Source

object AoCUtils {
  def readDemoFile(day: Int): List[String] = {
    val path = s"day$day/demo.txt"
    readFileInto(path)
  }
  def readInputFile(day: Int): List[String] = {
    val path = s"day$day/input.txt"
    readFileInto(path)
  }

  private def readFileInto(path: String): List[String] = {
    val resource = Source.fromResource(path)
    resource.getLines().toList
  }
}
