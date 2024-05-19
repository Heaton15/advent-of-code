package work

object day3 extends App {
  import scala.io.Source
  import scala.collection.mutable.ArrayStack
  import scala.collection.mutable.Stack
  import scala.collection.mutable.Queue
  import scala.collection.*

  val fileName = "inputs/elf_packages.txt"
  val packages = Source.fromFile(fileName).getLines.toList
  val prioList = (('a' to 'z') ++ ('A' to 'Z'))
    .zip(1 to 52)
    .map { case (letter, num) => Map(letter -> num) }
    .reduce((x, y) => x ++ y)

  def calcPriorities1(packages: List[String], prioList: Map[Char, Int]): Int = {
    val prioVal = packages.map { item =>
      val splitPackage = item.splitAt(item.length / 2)
      val (ruckSack1, ruckSack2) =
        (splitPackage._1.toSet, splitPackage._2.toSet)
      (ruckSack1 intersect ruckSack2).map { sharedVal =>
        prioList(sharedVal)
      }.toList
    }.flatten
    prioVal.sum
  }

  def calcPriorities2(packages: List[String], prioList: Map[Char, Int]): Int = {
    val groupedLists = packages.grouped(3).toList

    val badgeItem = groupedLists.map { item =>
      val intersect = calcIntersect(item)
      intersect
    }.flatten
    badgeItem.map { i => prioList(i) }.sum
  }

  def calcIntersect(myList: List[String]): Set[Char] = {
    if (myList.size == 1) { myList.head.toSet }
    else {
      val intersect = myList.head.toSet intersect calcIntersect(myList.tail)
      intersect
    }
  }

  val prioCount = calcPriorities1(packages, prioList)
  println(s"The priority sum is $prioCount")

  val prioCount2 = calcPriorities2(packages, prioList)
  println(s"The priority sum is $prioCount2\n")
}
