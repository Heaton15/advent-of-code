package work

object day4 extends App {
  import scala.io.Source
  import scala.collection.mutable.ArrayStack
  import scala.collection.mutable.Stack
  import scala.collection.mutable.Queue
  import scala.collection.*

  val fileName = "inputs/elf_cleaning_tasks.txt"
  val tasks = Source.fromFile(fileName).getLines.toList

  var overlap1 = 0
  var overlap2 = 0

  tasks.foreach { task =>
    val taskOrder = task.split(",")
    val (workLoad1, workLoad2) = (taskOrder.head, taskOrder.last)
    val (a, b) =
      (workLoad1.split("-").head.toInt, workLoad1.split("-").last.toInt)
    val (x, y) =
      (workLoad2.split("-").head.toInt, workLoad2.split("-").last.toInt)

    if ((y >= b && x <= a) || (b >= y && a <= x)) { overlap1 += 1 }
    if ((b >= x && a <= y)) { overlap2 += 1 }

  }

  println(s"The overlap count is $overlap1")
  println(s"The overlap count is $overlap2\n")
}
