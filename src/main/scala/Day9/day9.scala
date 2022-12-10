package Day9

import utils.AoCUtils

object day9 {

  case class Move(direction: Char, count: Int) {
    override def toString: String = s"Move $count steps in $direction"
  }
  object Move {
    def apply(line: String): Move = {
      val parts = line.split(" ").toList
      Move(parts(0).charAt(0), parts(1).toInt)
    }

    def moveOneStep(pos: Position, direction: Char): Position = {
      direction match {
        case 'U' => Position(pos.x, pos.y + 1)
        case 'D' => Position(pos.x, pos.y - 1)
        case 'R' => Position(pos.x + 1, pos.y)
        case 'L' => Position(pos.x - 1, pos.y)
      }
    }

  }

  case class Position(x: Int, y: Int)


  def applyMove(initialPosition: Position, move: Move): Position = {
    def rec(count: Int, positions: List[Position]): List[Position] = {
      if (count == 0)
        positions
      else {
        val pos = Move.moveOneStep(positions.head, move.direction)
        rec(count - 1, pos :: positions)
      }
    }
    rec(move.count, List(initialPosition)).head
  }

  def getListOfPositions(initialPosition: Position, moves: List[Move]): List[Position] = {

    def rec(positions: List[Position], remainingMoves: List[Move]): List[Position] = {
      if (remainingMoves.isEmpty)
        positions
      else {
        val curPos = positions.head
        val newPositions = getListOfPositionsFromOneMove(curPos, remainingMoves.head)
        rec(newPositions.reverse ++ positions, remainingMoves.tail)
      }
    }
    rec(List(initialPosition), moves).reverse
  }
  def getListOfPositionsFromOneMove(initialPosition: Position, move: Move): List[Position] = {
    def rec(count: Int, positions: List[Position]): List[Position] = {
      if (count == 0)
        positions
      else {
        val pos = Move.moveOneStep(positions.head, move.direction)
        rec(count - 1, pos :: positions)
      }
    }
    rec(move.count, List(initialPosition)).reverse.tail
  }
  def getTailPositionFromHeadPosition(tailPos: Position, headPos: Position): Position = {
    (Math.abs(headPos.x - tailPos.x), Math.abs(headPos.y - tailPos.y)) match {
      case (dx, dy) if (dx == 2 && dy == 0) =>
        Position(
          (tailPos.x + Math.signum(headPos.x - tailPos.x) * 1 ).toInt,
          tailPos.y
        )
      case (dx, dy) if (dx == 0 && dy == 2) =>
        Position(
          tailPos.x,
          (tailPos.y + Math.signum(headPos.y - tailPos.y) * 1 ).toInt,
        )
      case (dx,dy) if ((dx >= 2 || dy >= 2)) =>  {
        // Must the diagonal
        Position(
          (tailPos.x + Math.signum(headPos.x - tailPos.x) ).toInt,
          (tailPos.y + Math.signum(headPos.y - tailPos.y) ).toInt
        )
      }
      case _ => tailPos
    }

  }
  def getTailPositionsFromHeadPositions(initialTailPos: Position, headPositions: List[Position]): List[Position] = {
    def recurse(curTailPos: Position, headPositionsRemaining: List[Position], acc: List[Position]): List[Position] = {

      if (headPositionsRemaining.isEmpty)
        acc
      else {
        val curHeadPos = headPositionsRemaining.head
        val newTailPos = getTailPositionFromHeadPosition(curTailPos, curHeadPos)
        /*
        println("Current head: " + curHeadPos)
        println("Current tail: " + curTailPos)
        println("New     tail: " + newTailPos)
        println("--------------------")
         */
        recurse(newTailPos, headPositionsRemaining.tail, newTailPos :: acc)
      }
    }
    recurse(initialTailPos, headPositions, List.empty).reverse
  }
  def renderPosition(head: Position, tail: Position, dimX: Int, dimY: Int): Unit = {
    val linesOfPos = (1 to dimY).toList.reverse.map(y => (1 to dimX).toList.map(x => Position(x-1, y-1)))
    linesOfPos.foreach(line => {
      val temp = line.map(p => {
        if (p == head)
          " H "
        else if (p == tail)
          " T "
        else
          " . "
      })
      println(temp.mkString)
    })
  }
  def renderTrail(positions: List[Position], dimX: Int, dimY: Int): Unit = {
    val linesOfPos = (1 to dimY).toList.reverse.map(y => (1 to dimX).toList.map(x => Position(x-1, y-1)))
    linesOfPos.foreach(line => {

      val temp = line.map(p => {
        if (positions.contains(p))
          " # "
        else
          " . "
      })
      println(temp.mkString)
    })
  }

  def main(args: Array[String]): Unit = {
    //val lines = AoCUtils.readDemoFile(9)
    val lines = AoCUtils.readInputFile(9)
    val moves = lines.map(l => Move(l))


    val initialPosition
    val headPosition = Position(20, 20)
    val tailPosition = Position(20, 20)
    // Demo: First move is (R, 4), second i (U, 4)
    val headPositions = getListOfPositions(headPosition, moves)
    val tailPositions = getTailPositionsFromHeadPositions(Position(20,20), headPositions)
    println("***************** RESULT *******************")
    //println(moves)
    //println(headPositions)
    //println(tailPositions)
    renderTrail(headPositions, 30, 30)
    println("------------------------------------------")
    renderTrail(tailPositions, 30, 30)
    println("Unique positions of tail " + tailPositions.distinct.size)

    //Testing
    val headTest = Position(1,4)
    print("After move 1D: " + applyMove(headTest, Move('D', 1)))
    val tailTest = Position(2,4)

    //renderPosition(headTest, tailTest, 5,5)
    println("---- TEST--------------")
    println("Current head: " + headTest)
    println("Current tail: " + tailTest)

    println("New     tail: " + getTailPositionFromHeadPosition(tailTest, headTest))

    /* */
    //println(x)
    //println(moves)
  }
}
