package Day1
import scala.io.Source
import scala.collection.SortedSet

object calories {

  def sortSet[A](unsortedSet: Set[A])(implicit ordering: Ordering[A]): SortedSet[A] =
    SortedSet.empty[A] ++ unsortedSet

  def parseToSetOfList(entry: List[String], current: List[Int], acc: Set[List[Int]]): Set[List[Int]] = {
    if (entry.isEmpty) {
      acc
    } else if (entry.head == "") {
      parseToSetOfList(entry.tail, List.empty, acc.concat(Set(current)))
    } else {
      parseToSetOfList(entry.tail, entry.head.toInt :: current, acc)
    }
  }

  def main(args: Array[String]): Unit = {
    println("Hello, world")
    val lines = Source.fromFile("/Users/revsbech/Projects/LearningScala/AdventOfCode/src/main/scala/Day1/calories.txt").getLines().toList
    val sumCalories = parseToSetOfList(lines, List.empty[Int], Set.empty[List[Int]]).map(e => e.sum)
    println("Max calories: " + sumCalories.max)
    val sorted = sortSet(sumCalories)
    println("Sum of 3 highest elfs " + sorted.takeRight(3).sum)


  }
}
