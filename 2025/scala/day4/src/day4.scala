import scala.io.Source

object day4 extends App {
  val fileName = "inputs/day4.txt"
  val input = Source
    .fromFile(fileName)
    .getLines
    .toList

  // Nodes are first column, last column, first row, last row, corners
  // first column: i=0
  // last column: i=N
  // first row: j=0
  // last row: j=K
  // corners: i=N | 0, K | 0

  val maxCol = input.head.size
  val maxRow = input.size

  // i iterates over the row
  // j iterates over the column
  val paperRollGrid = input.map { r => r.split("").toList }

  case class Node(
      N: Boolean,
      E: Boolean,
      S: Boolean,
      W: Boolean,
      NE: Boolean,
      SE: Boolean,
      SW: Boolean,
      NW: Boolean,
      nodeIsPaper: Boolean
  ) {
    def fourOrFewer: Boolean = {
      Seq(N, E, S, W, NE, SE, SW, NW).count(_ == true) <= 3
    }
  }

  object Node {
    def isPaper(loc: String) = if (loc == "@") true else false

    def onGrid(row: Int, col: Int): Boolean = {
      if (row < 0 || row > maxRow - 1) {
        false
      } else if (col < 0 || col > maxCol - 1) {
        false
      } else {
        true
      }
    }

    def computeNode(rowIndex: Int, colIndex: Int, grid: List[List[String]]): Boolean = {
      onGrid(rowIndex, colIndex) && isPaper(grid(rowIndex)(colIndex))
    }

    def apply(rowIndex: Int, colIndex: Int, grid: List[List[String]]): Node = {
      new Node(
        N = computeNode(rowIndex - 1, colIndex, grid),
        E = computeNode(rowIndex, colIndex + 1, grid),
        S = computeNode(rowIndex + 1, colIndex, grid),
        W = computeNode(rowIndex, colIndex - 1,grid),
        NE = computeNode(rowIndex - 1, colIndex + 1,grid),
        SE = computeNode(rowIndex + 1, colIndex + 1, grid),
        SW = computeNode(rowIndex + 1, colIndex - 1, grid),
        NW = computeNode(rowIndex - 1, colIndex - 1, grid=grid),
        nodeIsPaper = isPaper(grid(rowIndex)(colIndex))
      )

    }

  }

  def display(disp: List[List[String]]) = disp.map(_.reduce(_ + _)).mkString("\n")

  def numValidPaperRolls(nodes: List[Node]): Int = nodes.map(n => n.nodeIsPaper && n.fourOrFewer).count(_ == true)

  def hasValidPaperRolls(nodes: List[Node]): Boolean = numValidPaperRolls(nodes) >= 1

  def stringToNode(s: List[List[String]]) = {
    s.zipWithIndex.map { case (row, i) =>
      val colParse = row.zipWithIndex.map { case (col, j) =>
        Node(i, j, s)
      }
      colParse
    }
  }

  def modifyNodes(inputStrings: List[List[String]]): List[List[String]] = {
    inputStrings.zipWithIndex.map { case (row, i) =>
      val colParse = row.zipWithIndex.map { case (col, j) =>
        val n = Node(i, j, inputStrings)
        if (n.nodeIsPaper && n.fourOrFewer) {
          "."
        } else {
          col
        }

      }
      colParse
    }
  }

  val part1 = stringToNode(paperRollGrid).flatten.map { n =>
    if (n.nodeIsPaper && n.fourOrFewer) { 1 }
    else { 0 }
  }.sum


  def reduceGraph(count: Int, graph: List[List[String]]): Int = {
    val nodes = stringToNode(graph).flatten
    if (! hasValidPaperRolls(nodes)) {
      count
    } else {
      val newCount = count + numValidPaperRolls(nodes)
      val newGraph = modifyNodes(graph)
      reduceGraph(newCount, newGraph)
    }
  }

  val part2 = reduceGraph(0, paperRollGrid)

  println(s"part1: $part1")
  println(s"part2: $part2")

}
