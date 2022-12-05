package Day5
import scala.util.matching.Regex
import scala.io.Source

object day5 {

  case class Stack(number: Int, items: List[Char]) {
    override def toString: String = {
      val x = items.reverse.mkString
      s"$number: $x"
    }
  }
  case class Move(from: Int, to: Int, num: Int)
  object Move {
    def fromString(input: String): Move = {
      val keyValPattern: Regex = "move ([0-9]+) from ([0-9]+) to ([0-9]+)".r
      keyValPattern.findFirstMatchIn(input).map(m => Move(m.group(2).toInt, m.group(3).toInt, m.group(1).toInt)).get
    }
  }

  def moveCasesInStacks(stacks: List[Stack], move: Move): List[Stack] = {
    // Reverse since we move one at a time!
    //val itemsToMove =stacks.find(s => s.number == move.from).get.items.take(move.num).toList.reverse.toList
    val itemsToMove =stacks.find(s => s.number == move.from).get.items.take(move.num).toList

    stacks.map(s => {
      if (s.number == move.from) {
        Stack(s.number, s.items.takeRight(s.items.size - itemsToMove.size))
      } else if (s.number == move.to) {
        Stack(s.number, itemsToMove ++ s.items)
      } else {
        s
      }
    })
  }

  def parseIntoStacks(lines: List[String]): List[Stack] = {
    val r = lines.reverse
    val numStacks = (r(0).length / 4.0).ceil.toInt
    val stackLines = lines.take(lines.length -1 ) //take away last lines
    (1 to numStacks).map(index => {
      Stack(index, extractStackfromLines(stackLines, index))
    }).toList
  }

  def parseStackLineIntoList(l: String): List[Char] = {
    println(l)
    val pattern: Regex = "([A-Z])".r
    pattern.findAllIn(l).toList.map(s => s(0))
  }

  def extractStackfromLines(lines: List[String], col: Int): List[Char] = {
    val index = 4*(col-1)+1
    lines.map(l => {
      if (l.length < index) None
      else if(l(index) == ' ') None
      else Some(l(index))
    }).collect({
      case Some(v) => v
    }).toList
  }
  def main(args: Array[String]): Unit = {
    val resource = Source.fromResource("day5/input.txt")
    val lines = resource.getLines().toList
    val firstBlockLines = lines.takeWhile(_ != "")

    val initialStacks = parseIntoStacks(firstBlockLines)
    println("------      Initial stack        ---------")
    initialStacks.map(println)
    val moves = lines.reverse.takeWhile(_ != "").reverse.map(l => Move.fromString(l))


    val endStacks = moves.foldLeft(initialStacks)((stacks, m) => {
      println("-------------------------------------------------")
      println(s"Moving top ${m.num} from stack ${m.from} to stack ${m.to}")

      val moved = moveCasesInStacks(stacks, m)
      moved.map(println)
      moved
    })
    println("=====================================================")
    println(initialStacks)
    println(endStacks.map(s => s.items(0)).mkString)
  }



}
