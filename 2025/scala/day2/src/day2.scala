import scala.io.Source

case class IdPoints(first: BigInt, last: BigInt)

object day2 extends App {
  val fileName = "inputs/day2.txt"
  val lockList = Source
    .fromFile(fileName)
    .getLines
    .toList

  val ranges = lockList.head.split(",").map { entry =>
    val splitEntry = entry.split("-").toSeq
    IdPoints(BigInt(splitEntry.head), BigInt(splitEntry.tail.head))
  }

  val rangeList: List[List[BigInt]] = ranges.map { range =>
    (range.first until range.last + 1).toList

  }.toList

  // Part 1
  val part1Ids = rangeList.flatMap { lst =>
    lst.map { num =>
      if (num.toString.size % 2 != 0) { BigInt(0) }
      else {
        val numHead = num.toString.take(num.toString.size / 2)
        val numTail = num.toString.drop(num.toString.size / 2)

        if (numHead == numTail) { num }
        else { BigInt(0) }
      }
    }
  }.sum

  // Part 2
  val part2Ids = rangeList.flatMap { lst =>
    lst.map { num =>

        val divRange = (1 until (num.toString.size / 2) + 1)
          .filter(i => num.toString.size % i == 0)
          .toSeq
          .reverse

        val maybeNum = divRange.map { div =>
          val isTrue = num.toString
            .grouped(div)
            .sliding(2)
            .map { num =>
              if (num.size > 1) { num.head == num.last }
              else { false }
            }
            .reduce(_ && _)
          
            if (isTrue) {num} else {BigInt(0)}
        }
        val result = maybeNum.dropWhile(_ == BigInt(0))
        if (result.isEmpty) {BigInt(0)} else {result.head}
    }
  }.sum

  println(s"part1: $part1Ids")
  println(s"part2: $part2Ids")
}
