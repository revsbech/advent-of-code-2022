package Day4

import scala.io.Source

object day4 {

  case class Range(begin: Int, stop: Int) {
    def fullyOverLaps(other: Range): Int = {
      if (other.begin <= begin && stop <= other.stop) {
        1
      } else if (begin <= other.begin && other.stop <= stop) {
        1
      } else {
        0
      }
    }

    def overlaps(other: Range): Int = {
      if ((other.stop < begin) || (other.begin > stop)) {
        0
      } else {
        1
      }
    }
  }
  object Range {
    def fromString(input: String) = {
      var elems = input.split("-").toList
      Range(elems(0).toInt, elems(1).toInt)
    }
  }

  def main(args: Array[String]): Unit = {
    val resource = Source.fromResource("day4/input.txt")
    val lines = resource.getLines().toList

    val x= lines.map(l => {
      var elems = l.split(",").toList

      val one = Range.fromString(elems(0))
      val two = Range.fromString(elems(1))
      one.overlaps(two)
    })
    println(x.sum)
  }
}
