package work

import scala.io.Source

object day8 extends App {

  val fileName = "inputs/elf_trees.txt"
  val storageData = Source.fromFile(fileName).getLines

  // A tree is visible if all trees between it and the edge are smaller than it
  // If a tree is 6 and it has 4 neighbors to the right at 3,2,4,5, then it is
  // visible from the right

  // All trees on the edge of the grid are visible since they aren't shield on
  // every side

  // 1. A tree is visible if all trees N, E, S, W of it are smaller than it
  // 2. All trees on the grid edge are visible
  // 3. If a tree node has anything greater than it in a cardinal direction, it
  //    won't be visible.

  // 1. Construct a tree grid of all nodes
  // The first pass is the northern boundary
  // The last pass is the southern boundary
  // The first of every pass is the wester boundary
  // The last of every pass is the eastern boundary

  class Tree(val height: Int, var N: Option[Tree], var E: Option[Tree], var S: Option[Tree], var W: Option[Tree]) {

    private def getN = this.N.map(i => i.height)
    private def getE = this.E.map(i => i.height)
    private def getS = this.S.map(i => i.height)
    private def getW = this.W.map(i => i.height)

    def surroundingTrees = println(s"N: $getN E: $getE S: $getS W: $getW")
  }

  def computeTreeGraph(file: List[String], nodeList: List[Tree]): List[List[Tree]] = {

    val row = file.head
    val trees = row.toList

    // North / South boundaries will be filled in when we parse columns
    def parseTreeRows(row: List[Char], lst: List[Tree]): List[Tree] = {
      val tree = row.head.asDigit
      // If we are at the end of the row, we are on the eastern boundary
      if (row.tail.isEmpty) {
        val newTree = new Tree(tree, None, None, None, Some(lst.last))
        lst.last.E = Some(newTree)
        lst :+ newTree
      }

      // If lst is empty, we are the north west boundary
      else if (lst.isEmpty) parseTreeRows(row.tail, lst :+ new Tree(tree, None, None, None, None))

      // Otherwise keep building W / E nodes
      else {
        val newTree = new Tree(tree, None, None, None, Some(lst.last))
        lst.last.E = Some(newTree)
        parseTreeRows(row.tail, lst :+ newTree)
      }
    }

    def parseTreeCols(treeList: List[Tree], lst: List[Tree]): List[Tree] = {
      val tree = treeList.head

      // If empty, we are on the northwest edge, return the standard tree
      if (lst.isEmpty) parseTreeCols(treeList.tail, lst :+ tree)

      // If there are no more trees, we are on the southern edge
      else if (treeList.tail.isEmpty) {
        tree.N = Some(lst.last)
        lst.last.S = Some(tree)
        lst :+ tree
      } else {
        tree.N = Some(lst.last)
        lst.last.S = Some(tree)
        parseTreeCols(treeList.tail, lst :+ tree)
      }
    }

    // Process the columns after we have the row graph
    def getCols(graph: List[List[Tree]], lst: List[List[Tree]]): List[List[Tree]] =
      graph.transpose.map(treeList => parseTreeCols(treeList, List.empty[Tree])).transpose

    val rowPass = file.map { row => parseTreeRows(row.toList, List.empty[Tree]) }
    val colPass = getCols(rowPass, List.empty)
    colPass
  }

  val treeNodes = computeTreeGraph(storageData.toList, List.empty)

  def solutions(graph: List[List[Tree]]): Unit = {
    // Now that we have the node graph, we can grab a node and traverse it and
    // decide if it is blocked or not. If it is visible in a direction, we can
    // just increment and move to the next one.

    // If the node has a None, it is on the periphery and clearly visible
    // If a node is taller than all of its neighbor nodes to the end, then it is visible

    def isVisible(tree: Tree): Boolean = {

      def checkNorth(innerTree: Tree, maybeVisible: Boolean = true): Boolean = {
        val result = innerTree.N match {
          case Some(t) => if (tree.height > t.height) checkNorth(t, true) else false
          case None    => maybeVisible
        }
        result
      }

      def checkEast(innerTree: Tree, maybeVisible: Boolean = true): Boolean = {
        val result = innerTree.E match {
          case Some(t) => if (tree.height > t.height) checkEast(t, true) else false
          case None    => maybeVisible
        }
        result
      }

      def checkSouth(innerTree: Tree, maybeVisible: Boolean = true): Boolean = {
        val result = innerTree.S match {
          case Some(t) => if (tree.height > t.height) checkSouth(t, true) else false
          case None    => maybeVisible
        }
        result
      }

      def checkWest(innerTree: Tree, maybeVisible: Boolean = true): Boolean = {
        val result = innerTree.W match {
          case Some(t) => if (tree.height > t.height) checkWest(t, true) else false
          case None    => maybeVisible
        }
        result
      }

      val n = checkNorth(tree)
      val e = checkEast(tree)
      val s = checkSouth(tree)
      val w = checkWest(tree)
      n || e || s || w
    }

    def treeScore(tree: Tree): Int = {
      def checkNorth(innerTree: Tree, currentScore: Int = 0): Int = {
        innerTree.N match {
          case Some(t) => if (tree.height > t.height) checkNorth(t, currentScore + 1) else currentScore + 1
          case None    => currentScore
        }
      }

      def checkEast(innerTree: Tree, currentScore: Int = 0): Int = {
        innerTree.E match {
          case Some(t) => if (tree.height > t.height) checkEast(t, currentScore + 1) else currentScore + 1
          case None    => currentScore
        }
      }

      def checkSouth(innerTree: Tree, currentScore: Int = 0): Int = {
        innerTree.S match {
          case Some(t) => if (tree.height > t.height) checkSouth(t, currentScore + 1) else currentScore + 1
          case None    => currentScore
        }
      }
      def checkWest(innerTree: Tree, currentScore: Int = 0): Int = {
        innerTree.W match {
          case Some(t) => if (tree.height > t.height) checkWest(t, currentScore + 1) else currentScore + 1
          case None    => currentScore
        }
      }
      val score = checkNorth(tree) * checkEast(tree) * checkSouth(tree) * checkWest(tree)
      if (score == 496650) println(tree.surroundingTrees)
      score
    }

    // val a = isVisible(graph.head.head)

    val numVisible = graph.flatMap(i => i.map(j => isVisible(j))).filter(_ == true).size
    println(s"Solution 1: Number of Visible Trees: $numVisible")

    // For solution 2, we need the viewing distance which is the multiplication
    // of all trees distances.

    // Edge trees automatically have a viewing distance of 0 since we multiply
    // by 0

    // Same height in a direction will count as 1 summed with the other
    // distances

    val maxTreeScore = graph.flatMap(i => i.map(j => treeScore(j))).max
    println(s"Max Tree Score: $maxTreeScore")

  } // end solution_1

  solutions(treeNodes)

}
