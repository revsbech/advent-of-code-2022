package Day9

import utils.AoCUtils

object day9 {

  case class Rope(knots: List[Position]) {
    def getHead = knots.head
    def getTail = knots.reverse.head

  }
  object Rope {
    def applyMove(rope: Rope, move: Move): Rope = {
      getRopesFromMove(rope, move).head
    }
    def getRopesFromMove(rope: Rope, move: Move): List[Rope] = {
      def rec(count: Int, ropes: List[Rope]): List[Rope] = {
        if (count == 0)
          ropes
        else {
          // Here is the beef, loop over rope knots and apply
          val curRope = ropes.head
          val newRope = moveRopeOneStep(curRope, move.direction)
          rec(count - 1, newRope :: ropes)
        }
      }
      rec(move.count, List(rope))
    }
    // Move entire rope one step in a direction
    def moveRopeOneStep(rope: Rope, direction: Char): Rope = {
      // Ex: T54321H
      // Start by moving head
      val newHead = Move.moveOneStep(rope.getHead, direction)
      // Ex T54321.H
      // Then do it for the next T5432
      // newHead= 6,5
      // rec ((6,5),List(5,5), Empty) =>
      // TH

      // Move each of the knots in the rope in tur with getTailPositionFromHeadPosition(tail, head)
      def rec(head: Position, knotsRemaing: List[Position], acc: List[Position]): List[Position] = {
        if (knotsRemaing.isEmpty)
          acc
        else {
          val firstKnot = knotsRemaing.head
          val newPos = getTailPositionFromHeadPosition(firstKnot, head)
          rec(newPos, knotsRemaing.tail, newPos :: acc)
        }
      }
      val knots = rec(newHead, rope.knots.tail, List.empty).reverse
      Rope(newHead :: knots)

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
    def getRopesFromMoves(initialRope: Rope, moves: List[Move], print: Boolean): List[Rope] = {
      def rec(movesRemaining: List[Move], acc: List[Rope]): List[Rope] = {
        if (movesRemaining.isEmpty)
          acc
        else {
          val curRope = acc.head
          val m = movesRemaining.head
          val ropesFromMove = getRopesFromMove(curRope, m)
          if(print) {
            println(s"================= ${m.direction} ${m.count} ===============")
            renderRope(ropesFromMove.head, 30, 30)
          }
          rec(movesRemaining.tail, ropesFromMove ++ acc)
        }
      }
      rec(moves, List(initialRope))
    }
    def getTailPositionsFromHeadPositions(initialTailPos: Position, headPositions: List[Position]): List[Position] = {
      def recurse(curTailPos: Position, headPositionsRemaining: List[Position], acc: List[Position]): List[Position] = {

        if (headPositionsRemaining.isEmpty)
          acc
        else {
          val curHeadPos = headPositionsRemaining.head
          val newTailPos = Rope.getTailPositionFromHeadPosition(curTailPos, curHeadPos)
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
  }

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
  object Position {
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
  }

  // ************** Utility function **************
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
  def renderRope(rope: Rope, dimX: Int, dimY: Int): Unit = {
    val linesOfPos = (1 to dimY).toList.reverse.map(y => (1 to dimX).toList.map(x => Position(x-1, y-1)))
    linesOfPos.foreach(line => {
      val temp = line.map(p => {
        // Single point render
        val i = rope.knots.lastIndexOf(p)
        if (i == 0)
          " H "
        else if (i == (rope.knots.length - 1))
          " T "
        else if (i >= 0)
          s" $i "
        else
          " . "
      })
      println(temp.mkString)
    })

  }
  // ************** Utility end function **************


  def main(args: Array[String]): Unit = {
    //val lines = AoCUtils.readDemoFile(9)
    val lines = AoCUtils.readInputFile(9)
    val moves = lines.map(l => Move(l))

    val initialPosition = Position(12,12)
    val initialRope = Rope((1 to 10).toList.map(_ => initialPosition))
    renderRope(initialRope, 30,30)
    println("-----------------------")
    //val newRope = Rope.applyMove(initialRope, moves(0))

    val ropeHistory = Rope.getRopesFromMoves(initialRope, moves, false)
    val newRope = ropeHistory.head
    renderRope(newRope, 30, 30)
    println(newRope)
    println("-----------------------")
    val tailPositions = ropeHistory.map(rope => rope.getTail)
    renderTrail(tailPositions, 50, 50)
    println("Unique positions of tail " + tailPositions.distinct.size)
    //val headPositions = Position.getListOfPositions(initialPosition, moves)
    //val tailPositions = Rope.getTailPositionsFromHeadPositions(initialPosition, headPositions)

    println("***************** RESULT *******************")
    //println(moves)
    //println(headPositions)
    //println(tailPositions)
    /*
    renderTrail(headPositions, 50, 50)
    println("------------------------------------------")
    renderTrail(tailPositions, 50, 50)
    println("Unique positions of tail " + tailPositions.distinct.size)
    */
    //Testing
    //val headTest = Position(1,4)
    //print("After move 1D: " + Position.applyMove(headTest, Move('D', 1)))
    //val tailTest = Position(2,4)

    //renderPosition(headTest, tailTest, 5,5)
    //println("---- TEST--------------")
    //println("Current head: " + headTest)
    //println("Current tail: " + tailTest)

    //println("New     tail: " + Rope.getTailPositionFromHeadPosition(tailTest, headTest))

    /* */
    //println(x)
    //println(moves)
  }
}
