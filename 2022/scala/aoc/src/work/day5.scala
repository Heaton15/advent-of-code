package work
object day5 extends App {
  import scala.io.Source
  import scala.collection.mutable.ArrayStack
  import scala.collection.mutable.Stack
  import scala.collection.mutable.Queue
  import scala.collection.*

  def grabStack(fileName: List[String]): List[ArrayStack[Char]] = {
    // Function grabs the number of rows that are in use for the current
    // stackup, grabs the crate data in each column, and then returns a List of
    // the crate data for each row

    def stackBottom(currFile: List[String]): Int = {
      if (currFile.head(1) == '1') { (fileName.size - 1) - currFile.size }
      else { stackBottom(currFile.tail) }
    }

    def currStack(
        fileName: List[String],
        base: Int
    ): List[ArrayStack[Char]] = {
      def iterStack(fileName: List[String], newBase: Int): List[List[Char]] = {
        val currStack = {
          if (newBase == 0) {
            List((1 to fileName(newBase).size by 4).map { i =>
              fileName(newBase)(i)
            }.toList)
          } else {
            val myVal = (1 to fileName(newBase).size by 4).map { i =>
              fileName(newBase)(i)
            }.toList :: iterStack(fileName, newBase - 1)
            myVal
          }
        }
        currStack
      }
      val currStackup = iterStack(fileName, base)

      val bufferList = (0 to currStackup.head.size - 1).map { i =>
        ArrayStack[Char]()
      }

      currStackup.foreach { case row =>
        row.zipWithIndex.foreach { case (ele, i) =>
          if (ele != ' ') bufferList(i) += ele
        }
      }

      bufferList.toList
    }

    val bottom = stackBottom(fileName)
    val myStack = currStack(fileName, bottom)
    myStack
  }

  def grabInstructions(fileName: List[String]): List[List[String]] = {
    val instructions = (10 to fileName.size - 1).map { i =>
      val splitLine = fileName(i).split(" ")
      List(splitLine(1), splitLine(3), splitLine(5))
    }
    instructions.toList
  }

// Everything up to here is just code needed to read the text file crate
// stackup and movement instructions 0_0

  val fileName = "inputs/elf_crane.txt"
  val workload = Source.fromFile(fileName).getLines.toList

  val crateStackup = grabStack(workload)
  val instructions = grabInstructions(workload)

// Crane Master 9000
  instructions.foreach { cmd =>
    val move = cmd.head.toInt
    val start = cmd(1).toInt - 1
    val finish = cmd.last.toInt - 1

    Seq.tabulate(move)(_ + 1).foreach { _ =>
      crateStackup(finish).push(crateStackup(start).head)
      crateStackup(start).pop
    }
  }
  val lastCrates = crateStackup.map { stack => stack.head }.mkString("")
  println(s"Crane 9000 Crate Stackup: $lastCrates\n")

// Crane Master 9001

  val crateStackup9001 = grabStack(workload)

  instructions.foreach { cmd =>
    val move = cmd.head.toInt
    val start = cmd(1).toInt - 1
    val finish = cmd.last.toInt - 1
    var holdStackup = ArrayStack[Char]()

    Seq.tabulate(move)(_ + 1).foreach { _ =>
      holdStackup.push(crateStackup9001(start).head)
      crateStackup9001(start).pop
    }

    holdStackup.foreach { i =>
      crateStackup9001(finish) push i
    }

  }
  val lastCrates9001 = crateStackup9001.map { stack => stack.head }.mkString("")
  println(s"Crane 9001 Crate Stackup: $lastCrates9001\n")
}
