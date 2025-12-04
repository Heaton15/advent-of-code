import scala.io.Source

object day3 extends App {
  val fileName = "inputs/day3.txt"
  val banks = Source
    .fromFile(fileName)
    .getLines
    .toList


    val joltageList = banks.map{b => 
      val joltageLevels = (0 until 10).toList.reverse

      val part1 = joltageLevels.map {j => 
        val firstIndex = b.zipWithIndex.filter(_._1 == j.toChar).map(_._2)
        println(s"$j, $firstIndex")

        // If multiple hits exist for a single number, then 99, 88, 77, is the highest
        if (firstIndex.size >= 2) {
          j*11
        } else if (firstIndex.size == 1) {
          val secondLargest = b.drop(firstIndex.head+1).toList.map(_.toInt).max
          s"${j}${secondLargest}".toInt
        } else {
          0
        }
      }.dropWhile(_ == 0)
      part1
    }



}
