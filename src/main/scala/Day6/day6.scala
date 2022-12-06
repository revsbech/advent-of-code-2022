package Day6

import utils.AoCUtils

object day6 {

  def isUnique(chars: List[Char]) = {
    chars.distinct.length == chars.length
  }
  val seqLength = 14;
  def traverseTest(input: List[Char], curIndex: Int): Int = {
    val elem = input.take(seqLength)
    if (isUnique(elem)) curIndex + seqLength
    else traverseTest(input.tail, curIndex + 1)
  }

  def main(args: Array[String]): Unit = {
    val lines = AoCUtils.readInputFile(6)
    lines.foreach(l => {
      println("Seq: " + l)
      println("Index is: " + traverseTest(l.toList, 0))
    })
  }
}
