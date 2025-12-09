import scala.io.Source

object day3 extends App {
  val fileName = "inputs/day3.txt"
  val banks = Source
    .fromFile(fileName)
    .getLines
    .toList

  val part1 = banks.map { b =>
    val joltageLevels = (0 until 10).toList.reverse

    val part1 = joltageLevels
      .map { j =>
        val firstIndex =
          b.zipWithIndex.filter(_._1.toString == j.toString).map(_._2)

        val result =
          if (firstIndex.size >= 1 && !b.drop(firstIndex.head + 1).isEmpty) {
            val secondLargest =
              b.drop(firstIndex.head + 1).toList.map(_.toString.toInt).max
            s"${j}${secondLargest}".toInt
          } else {
            0
          }
        result
      }
      .dropWhile(_ == 0)
    part1.head
  }.sum

  // part 2
  // 1. Grab largest index
  // 2. Grab second largest index, check tail. If not big enough or ==, check
  //    next largest index. Repeat until tail is == or >

  trait AocArray

  case class SearchArray(
      remainder: List[String],
      requiredNextSize: Int,
      currStrNum: String
  ) extends AocArray {
    def skip: Boolean = false

  }

  case class EmptyArray() extends AocArray

  def part2(banks: List[String], scan: Int): BigInt = {

    def findNextNumber(s: SearchArray): BigInt = {
      val tmp = (0 until 10).toList.reverse
        .map { i =>
          val largestNumIndex =
            s.remainder.zipWithIndex.filter(_._1 == i.toString).map(_._2)

          if (
            !largestNumIndex.isEmpty && s.remainder
              .drop(largestNumIndex.head + 1)
              .size >= s.requiredNextSize - 1
          ) {
            if (s.requiredNextSize == 0) {
              EmptyArray()
            } else {
              SearchArray(
                s.remainder.drop(largestNumIndex.head + 1),
                s.requiredNextSize - 1,
                s.currStrNum ++ s.remainder(largestNumIndex.head).toString
              )
            }

          } else {
            EmptyArray()
          }
        }

      val nextLargest = tmp.collect { case sa: SearchArray => sa }
      if (nextLargest.isEmpty) { BigInt(s.currStrNum) }
      else {
        findNextNumber(nextLargest.head)
      }
    }

    val allBanks = banks.map { b =>
      findNextNumber(SearchArray(b.toList.map(_.toString), scan, ""))
    }.sum
    allBanks
  }

  println(s"part1: $part1")
  println(s"part2: ${part2(banks, scan = 12)}")
}
