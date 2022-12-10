package Day8

import utils.AoCUtils

object day8 {

  case class Forrest(items: List[List[Tree]]) {
    override def toString: String = {
      items.map(lineOfTrees => {
        val trees = lineOfTrees.map(t => t.height)
        trees.mkString
      }).mkString("\n")
    }
    def size: Int = items.size // Assuming it is square...
    def getLeftTrees(tree: Tree): List[Tree] = {
      val line = items(tree.row)
      line.take(tree.col).reverse
    }
    def getRightTrees(tree: Tree): List[Tree] = {
      val line = items(tree.row)
      line.takeRight(line.size - tree.col - 1)
    }
    def getAboveTrees(tree: Tree): List[Tree] = {
      val t = items.transpose
      val line = t(tree.col)
      line.take(tree.row).reverse
    }
    def getBelowTrees(tree: Tree): List[Tree] = {
      val t = items.transpose
      val line = t(tree.col)
      //println(s"$tree line)
      line.takeRight(line.size - tree.row - 1)
    }

    def getAllTrees(): List[Tree] = {
      items.flatten
    }
  }
  object Forrest {
    def getTreesInSight(forrest: Forrest): List[Tree] = {
      val first = forrest.items.flatMap(trees => getTreesInSightForOneLine(trees) ++ getTreesInSightForOneLine(trees.reverse))
      val second = forrest.items.transpose.flatMap(trees => getTreesInSightForOneLine(trees) ++ getTreesInSightForOneLine(trees.reverse))
      first ++ second
    }
    def getVisibleTrees(me: Tree, trees: List[Tree]) : List[Tree] = {


      //def rec()
      //////////////////////////// I GOT HERE, I need to recurse into the trees, and only select the trees up smaller until i hit one same size
      def rec(remain: List[Tree], acc: List[Tree]): List[Tree]  = {
        if (remain.isEmpty)
          acc
        else {
          val t = remain.head
          if (t.height >= me.height)
            t :: acc
          else
            rec(remain.tail, t :: acc)
        }
      }
      rec(trees, List.empty)


    }
    def getScenicScoreOfTree(forrest: Forrest, tree: Tree): Int = {
      // If a the edge, the viewing distance in that dir will be 0, so scenic sciopr will also be 0
      if (tree.id._1 == 0 || tree.id._1 == forrest.size || tree.id._2 == 0 || tree.id._2 == forrest.size)
        0
      else {
        val upScore = Forrest.getVisibleTrees(tree, forrest.getAboveTrees(tree)).size
        val downScore = Forrest.getVisibleTrees(tree, forrest.getBelowTrees(tree)).size
        val rightScore = Forrest.getVisibleTrees(tree, forrest.getRightTrees(tree)).size
        val leftScore = Forrest.getVisibleTrees(tree, forrest.getLeftTrees(tree)).size
        val score = upScore * downScore * rightScore * leftScore
        //println(s"Tree: (${tree.row}, ${tree.col}) height ${tree.height} Total: $score Scores (Up:$upScore Down: $downScore Right: $rightScore Left $leftScore" + ")")
        score
      }
    }

    def getTreesInSightForOneLine(trees: List[Tree]): List[Tree] = {
      def rec(remainingTrees: List[Tree], highestSoFar: Tree, current: List[Tree] ): List[Tree] = {
        if (remainingTrees.isEmpty)
          current
        else {
          val curTree= remainingTrees.head
          val newHightestTree = if (curTree.height > highestSoFar.height) curTree else highestSoFar
          rec(remainingTrees.tail, newHightestTree, if (curTree.height > highestSoFar.height) curTree :: current else current)
        }
      }
      rec(trees, trees.head, List(trees.head)).reverse

    }
  }

  case class Tree(id: (Int, Int), height: Int) {
    val row = id._1
    val col = id._2

    override def toString: String = s"($row, $col) => $height"
  }

  def countTreesInSight(list: List[Int], highestSoFat: Int, current: Int): Int = {
    if (list.isEmpty)
      current
    else {
      val treeHeight = list.head
      countTreesInSight(list.tail,
        if (treeHeight > highestSoFat) treeHeight else highestSoFat,
        if (treeHeight > highestSoFat) current + 1 else current,
      )
    }
  }

  def countTreesInSightForOneLine(list: List[Int]): Int = {
    countTreesInSight(list, 0, 0) + countTreesInSight(list.reverse, 0, 0)
  }

  def countTressInSightInMatrix(m: List[List[Int]]): Int = {
    m.map(line => countTreesInSightForOneLine(line)).sum
  }


  def test(m: List[List[Int]]): Int = {
    //def recurse(m: L)
    1
  }

  type Position = (Int, Int)
  def main(args: Array[String]): Unit = {
    val lines = AoCUtils.readInputFile(8)
    //val lines = AoCUtils.readDemoFile(8)

    val matrix: List[List[Int]] = lines.map(l => l.toList.map(_.toString.toInt))
    val tempMatrix: List[List[Tree]] = lines.zipWithIndex.map(elem => {
      val line = elem._1
      val lineNumber = elem._2
      line.toList.zipWithIndex.map(t => {
        Tree((lineNumber, t._2), t._1.toString.toInt)
      })
    })
    val forrest = Forrest(tempMatrix)
    println(forrest)
    println("Visible trees: " + Forrest.getTreesInSight(forrest).distinct.size)
    println("-------------- Ex 2------------------")
    //val testTree = Tree((3,2), 5)
    //val treeLine = forrest.getBelowTrees(testTree)
    //println("Treesline: " + treeLine)
    //println("Visible trees: " +Forrest.getVisibleTrees(testTree, treeLine))
    //println("Score: " + Forrest.getScenicScoreOfTree(forrest, testTree))

    val scenicScores = forrest.getAllTrees().map(tree => Forrest.getScenicScoreOfTree(forrest, tree))
    println("Max score is " + scenicScores.max)

  }
}
