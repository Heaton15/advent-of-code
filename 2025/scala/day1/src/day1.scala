import scala.io.Source
import scala.collection.mutable.ArrayStack
import scala.collection.mutable.Stack
import scala.collection.mutable.Queue
import scala.collection.*

object day1 extends App {
  val fileName = "inputs/day1.txt"
  val lockList = Source
    .fromFile(fileName)
    .getLines
    .toList

  def turnDial(currDial: Int, instruction: String): (Int, Int) = {
    val rotateDir = instruction.head
    val turnCount = instruction.tail.toInt

    val newDialNumber = rotateDir match {
      case 'R' => {
        val newDialNumber = currDial + turnCount
        newDialNumber % 100
      }
      case 'L' => {
        val newDialNumber = currDial - turnCount
        newDialNumber % 100
      }
      case _ => throw AssertionError("Invalid Instruction! Cry about it.")
    }

    val isZero = if (newDialNumber == 0) 1 else 0
    (isZero, newDialNumber)

  }

  def numZeroPoints(currDial: Int, instruction: String): Int = {
    val rotateDir = instruction.head
    val turnCount = instruction.tail.toInt

    val zeroCount = rotateDir match {
      case 'R' => {
        var runningNum = currDial
        var numPasses = 0
        val newDialNum = currDial + turnCount

        while (runningNum < newDialNum) {
          runningNum += 1
          if (runningNum % 100 == 0) numPasses += 1
        }
        numPasses
      }
      case 'L' => {
        if (turnCount < currDial) {
          0
        } else if (turnCount == currDial) { 1 }
        else {
          val newDialNum = currDial - turnCount
          var runningNum = currDial
          var numPasses = 0
          while (runningNum > newDialNum) {
            runningNum -= 1
            if (runningNum % 100 == 0) numPasses += 1
          }

          numPasses

        }

      }
      case _ => throw AssertionError("Invalid Instruction! Cry about it.")
    }
    zeroCount
  }

  def computeZeros_part1(
      dialStart: Int,
      instr: List[String],
      numZeros: Int
  ): Int = {
    if (instr.isEmpty) {
      numZeros
    } else {
      val (isZero, newDialNumber) = turnDial(dialStart, instr.head)
      val newZeroCount = numZeros + isZero
      computeZeros_part1(newDialNumber, instr.tail, newZeroCount)
    }
  }

  def computeZeros_part2(
      dialStart: Int,
      instr: List[String],
      numZeros: Int
  ): Int = {
    if (instr.isEmpty) {
      numZeros
    } else {
      val (_, newDialNumber) = turnDial(dialStart, instr.head)
      val zeroCounts = numZeroPoints(dialStart, instr.head)
      val newZeroCount = zeroCounts + numZeros
      println(
        s"currDial: $dialStart, newDial: $newDialNumber, (${instr.head}) $numZeros -> $newZeroCount"
      )
      computeZeros_part2(newDialNumber, instr.tail, newZeroCount)
    }
  }

  val day1_p1 = computeZeros_part1(50, lockList, 0)
  val day1_p2 = computeZeros_part2(50, lockList, 0)

  println(s"Zeros: $day1_p1")
  println(s"Zeros: $day1_p2")

}
