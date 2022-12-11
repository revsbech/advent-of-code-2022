package Day6

import utils.AoCUtils

object day7 {

  sealed trait Node {
    val name: String
  }
  case class File(name: String, size: Int) extends Node
  case class Dir(name: String, items: List[Node]) extends Node {

  }
  object Dir {
    def addNode(target: Dir, node: Node): Dir = {
      Dir(target.name, target.items ++ List(node))
    }
  }

  def recurseTree(node: Node, op: (Node, Int) => Unit, depth: Int = 0): Unit = {
    node match {
      case File(name, size) => op(node, depth)//println(s"$spacer-$name (size=$size)")
      case Dir(name, items) => {
        op(node, depth)
        items.foreach(n => recurseTree(n, op, depth + 1))
      }
    }
  }
  // Select dirs that fulfill the selecotr criteria
  def extractDirs(node: Node, selector: Dir => Boolean, accumulator: List[Dir]): List[Dir] = {
    node match {
      case File(_,_) => accumulator
      case dir @ Dir(name, dirs) => {
        val sub = accumulator ++ dirs.flatMap(sn => {
          extractDirs(sn, selector, accumulator)
        })
        if (selector(dir))
          List(dir) ++ sub
        else sub
      }
    }
  }
  // Get size of a nodes byt recursing into it
  def getSize(node: Node): Int = {
    node match {
      case File(name, size) => size
      case Dir(name, items) => items.map(getSize).sum
    }
  }



  case class LineWithDepth(line: String, depth: Int, parentDir: Option[String], fullPat: Option[String])
  // Curpath is the depth a < b < c in reverse order, so a dir with
  // c
  // --c
  // ----d
  def parseLinesIntoLinesWithDepth(lines: List[String], acc: List[LineWithDepth], currentPath: List[String], curDepth: Int = 0, curLine: Int =1): List[LineWithDepth] = {

    if (lines.isEmpty) {
      acc
    } else {
      val line = lines.head
      val parts = line.split(" ")
      if (parts(0) == "$") {
        if (parts(1) == "cd") {
          if (parts(2) == "..") {
            // Would fail if cd .. on root dir, wont handle
            //println(s"[$curLine] CD .. Curdepth $curDepth CurPath " + currentPath.mkString("/"))
            parseLinesIntoLinesWithDepth(lines.tail, acc, currentPath.tail, curDepth - 1, curLine + 1)
          } else {
            // Cd into actual dir
            //println(s"[$curLine] CD ${parts(2)} Curdepth $curDepth CurPath " + currentPath.mkString("/"))
            val parentElem = if (currentPath.isEmpty) None else Some(currentPath.head)
            parseLinesIntoLinesWithDepth(lines.tail, acc, parts(2) :: currentPath, curDepth +1,curLine + 1)
          }
        } else {
          // Must be ls
          //println(s"[$curLine] LS <> Curdepth $curDepth")
          parseLinesIntoLinesWithDepth(lines.tail, acc, currentPath, curDepth,curLine + 1)
        }
      } else {
        // will be dir <name> or 1234 filename

        val key = currentPath.mkString("/")
        //println(s"[$curLine] ITEM ${parts(0)} (${parts(1)}  Curdepth $curDepth Key: " + key)
        val item = LineWithDepth(line , currentPath.length, Some(currentPath.head), Some(key))
        parseLinesIntoLinesWithDepth(lines.tail, item :: acc, currentPath, curDepth,curLine + 1)
      }
    }
  }

  /**
   * Step 0:
   *  nodesOnCurrentLevel: List(LineWithDepth(584 i,3,Some(e))
   *  remoining ....(long lines)
   *  Calls recursive with (depth -1, Dir("e", List(Fil("i", 584))
   * Step 2:
   *  nodesOnCurrentLevel: LineWithDepth(dir e,2,Some(a)), LineWithDepth(29116 f,2,Some(a)), LineWithDepth(2557 g,2,Some(a)), LineWithDepth(62596 h.lst,2,Some(a)), LineWithDepth($ cd e,2,Some(a)), LineWithDepth(4060174 j,2,Some(d)), LineWithDepth(8033020 d.log,2,Some(d)), LineWithDepth(5626152 d.ext,2,Some(d)), LineWithDepth(7214296 k,2,Some(d)),
   *  Generates list
   *    List(Dir("a", files), Dir("b", files))
   *
   */
  def buildFromBottom(
                       depth: Int,
                       remaining: List[LineWithDepth],
                       linesByParent: Map[Option[String], List[LineWithDepth]],
                       processedNodesByFullPath: Map[String, Dir]
        ): List[Node] = {

    val (linesOnCurrentDepth, rest) = remaining.span(_.depth == depth)
    //println(s"ProcessedNodesByFullPath Depth: $depth")
    //println(processedNodesByFullPath)
    val linesOnCurrentDepthByFullPath = linesOnCurrentDepth.sortBy(_.fullPat).groupBy(_.fullPat)
    val newProccessed = linesOnCurrentDepthByFullPath.map {
      case (Some(fullPath), lines) =>
        val nodes: List[Node] = lines.map(l => {
          val parts = l.line.split(" ")
          if (parts(0) == "dir") {
            //processedNodesByFullPath.get(parts(1) + "/" + l.fullPat.get).getOrElse(Dir("empty", List.empty))
            //println(l.line)
            //val temp = parts(1) + "/" + l.fullPat.get
            val key = parts(1) + "/" + l.fullPat.get
            //println(s"Searching for $key at Depth ${l.depth}")
            //println(processedNodesByFullPath.keys.toList)
            processedNodesByFullPath.get(key).get
          } else {
            File(parts(1), parts(0).toInt)
          }
        })
        val x = fullPath.split("/")
        (fullPath -> Dir(x.headOption.getOrElse("/"), nodes))
    }
    // List[Dir]

    //newProccessed.toList

    if (depth > 1) {
      buildFromBottom(depth - 1, rest, linesByParent, newProccessed ++ processedNodesByFullPath)
    } else {
      newProccessed.values.toList
      //newProccessed
    }

     /**/

  }

  def main(args: Array[String]): Unit = {
    def printNode(n: Node, depth:Int): Unit = {
      val spacer = " " * depth
      n match {
        case File(name, size) => println(s"$spacer- $name (size=$size)")
        case Dir(name, _) => {
          val size = getSize(n)
          println(s"$spacer- $name (dir, total size $size)")
        }
      }
    }

    //val lines = AoCUtils.readDemoFile(7)
    val lines = AoCUtils.readInputFile(7)
    val parts = parseLinesIntoLinesWithDepth(lines, List.empty, List.empty, 0)
    val linesByParent = parts.groupBy(l => l.parentDir)
    val linesSortedByDepth = parts.sortBy(_.depth).reverse
    //val linesSortedByParent = parts.sortBy(_.parentDir).reverse
    //println(linesSortedByDepth)
    val depth = linesSortedByDepth.head.depth
    val filesystem = buildFromBottom(depth, linesSortedByDepth, linesByParent, Map.empty).head

    recurseTree(filesystem, printNode)

    println()

    /*
    val dirs =extractDirs(test, d => (getSize(d) <= 100000), List.empty)
    dirs.map(d => {
      println(s"Dir: ${d.name} ${getSize(d)}")
    })
    println("Total size of dirs that match: " + dirs.foldRight(0)((d, acc) => acc + getSize(d)))
     */
    val updateRequiredSpace = 30000000
    val availableSpace = 70000000
    val usedSpace = getSize(filesystem)
    val freeSpace = availableSpace - usedSpace
    val requiredSpace = updateRequiredSpace - freeSpace

    println("Total space used: " + getSize(filesystem))
    println(s"Finding dir that feeds up $requiredSpace bytes")

    val dirs =extractDirs(filesystem, d => (getSize(d) >= requiredSpace), List.empty)
    val relevantDirs = dirs.sortBy(dir => getSize(dir))

    println("Dirs that match: ")
    relevantDirs.map(d => {
      println(s"Dir: ${d.name} ${getSize(d)}")
    })

    println("Smalles dir that fullfilles space need: " + relevantDirs.head.name + " (Size: " + getSize(relevantDirs.head)+ ") ")
    val test = dirs.find(d => getSize(d) > requiredSpace).get
    println("-- Smallest dir that fullfilles space need: " + test + " (Size: " + getSize(test)+ ") ")


  }

}
