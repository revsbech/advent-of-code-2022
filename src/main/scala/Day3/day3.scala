package Day3

import scala.io.Source

object day3 {

  case class RuckSack(itemLine: String) {
    def getItems(): List[Char] = itemLine.toList
    def findDuplicate(): Char = {
      val items = getItems()
      val firstHalf = items.take(items.length / 2)
      val lastHalf = items.takeRight(items.length / 2)
      firstHalf.intersect(lastHalf).last
    }
  }

  /**
   * Return the point for a -> z:  1 -> 26, A -> Z: 27 -> 52
   */
  def mapValue(c: Char): Int = {
    val asciiVal = c.toInt
    if (asciiVal > 90) c.toInt - 96
    else c.toInt - 38
  }

  def findIntersectInRucksacks(r1: RuckSack, r2: RuckSack, r3: RuckSack): Char = {
    r1.getItems().intersect(r2.getItems()).intersect(r3.getItems()).last
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("/Users/revsbech/Projects/LearningScala/AdventOfCode/src/main/scala/Day3/input.txt").getLines().toList
    val ruckSacks = lines.map(RuckSack)
    val duplicates = ruckSacks.map(r => r.findDuplicate())
    println("Ex 1: " + duplicates.map(mapValue).sum)

    val groups = ruckSacks.grouped(3).toList
    val temp = groups.map(g =>
      findIntersectInRucksacks(g(0), g(1), g(2))
    )
    println("Ex2: " + temp.map(mapValue).sum)
  }
}
