import scala.io.Source
import scala.collection.mutable.ArrayStack
import scala.collection.mutable.Stack
import scala.collection.mutable.Queue
import scala.collection.*

val fileName = "inputs/elf_food_amounts.txt"
val elfFoodList = Source
  .fromFile(fileName)
  .getLines
  .toList

// List(5)
def getElfCalories(elfList: List[String]): List[Int] = {
  val (elf, newList) = elfList.span(i => i != "")
  val calTotal = elf.foldLeft(0)(_.toInt + _.toInt)
  if (newList.isEmpty) { List(calTotal) }
  else {
    val calList = calTotal :: getElfCalories(newList.tail)
    calList
  }
}

val foodList = getElfCalories(elfFoodList)
val calories = foodList.max
println(s"The max elf carries $calories calories")
val sortedElves = foodList.sorted
val caloriesThreeElves = sortedElves.takeRight(3).sum
println(s"The max three elves carries $caloriesThreeElves calories\n")
