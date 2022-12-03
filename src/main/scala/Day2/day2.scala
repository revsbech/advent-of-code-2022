package Day2
import scala.io.Source

object day2 {
  /**
   * A: Rock
   * B: Paper
   * C: Scissors
   *
   * x: Rock
   * y: Paper
   * z: Scissors
   *
   * Points:
   *  - Shapes: Rock: 1, Paper: 2, Scissors: 3
   *  - Outcome: Loose: 0, draw: 3, Win: 6
   */
  sealed trait Move {
    def getPoints: Int
  }
  object Move {
    def apply(c: Char) = {
      c match {
        case 'A' => Rock
        case 'B' => Paper
        case 'C' => Scissors
        case 'X' => Rock
        case 'Y' => Paper
        case 'Z' => Scissors
      }
    }
  }
  case object Rock extends Move {
    def getPoints: Int = 1
  }
  case object Paper extends Move {
    def getPoints: Int = 2
  }
  case object Scissors extends Move {
    def getPoints: Int = 3
  }


  case class Round(opponent: Move, mine: Move) {
    def pointsFromWin(): Int = {
      // 0: Loos, 3 Dra 6 win
      (opponent, mine) match {
        case (Rock, Rock) => 3
        case (Rock, Paper) => 6
        case (Rock, Scissors) => 0

        case (Paper, Rock) => 0
        case (Paper, Paper) => 3
        case (Paper, Scissors) => 6

        case (Scissors, Rock) => 6
        case (Scissors, Paper) => 0
        case (Scissors, Scissors) => 3
      }
    }
    def score(): Int = {
      val fromSelection = mine.getPoints
      fromSelection + pointsFromWin()
    }

  }


  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("/Users/revsbech/Projects/LearningScala/AdventOfCode/src/main/scala/Day2/input.txt").getLines().toList
    val rounds = lines.map(l => {
      val opp = l.charAt(0)
      val my = l.charAt(2)
      Round(Move(opp), Move(my))
    })
    val test: Move = Move('Y')
    println(" **** Dec 2th ******")
    println("Exercise 1: " + rounds.foldRight[Int](0)((round, cur) => cur + round.score()))


    val roundNew = lines.map(l => {
      val opp = Move(l.charAt(0))
      val outcome = l.charAt(2)
      val myMove: Move = (opp, outcome) match {
        // Need to loose
        case (Rock, 'X') =>  Scissors
        case (Paper, 'X') =>  Rock
        case (Scissors, 'X') =>  Paper

        // Need to make a draw
        case (Rock, 'Y') =>  Rock
        case (Paper, 'Y') =>  Paper
        case (Scissors, 'Y') =>  Scissors

        // Need to win
        case (Rock, 'Z') =>  Paper
        case (Paper, 'Z') =>  Scissors
        case (Scissors, 'Z') =>  Rock

      }
      Round(opp, myMove)
    })

    println("Exercise 2: " + roundNew.foldRight[Int](0)((round, cur) => cur + round.score()))
  }
}
