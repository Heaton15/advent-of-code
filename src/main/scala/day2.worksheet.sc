import scala.io.Source
import scala.collection.mutable.ArrayStack
import scala.collection.mutable.Stack
import scala.collection.mutable.Queue
import scala.collection.*

val fileName = "inputs/elf_rps.txt"
val elfRPSKey = Source.fromFile(fileName).getLines.toList

// Create a Key for RPS Points
val resultsKey = Map(
  "x" -> 1,
  "y" -> 2,
  "z" -> 3
)
val playKey = Map(
  "x" -> "rock",
  "y" -> "paper",
  "z" -> "scissors",
  "a" -> "rock",
  "b" -> "paper",
  "c" -> "scissors"
)

def calcRpsWinner(enemyPlay: String, myPlay: String): Int = {
  if (playKey(enemyPlay) == playKey(myPlay)) { 3 }
  else {
    if (playKey(enemyPlay) == "rock") {
      if (playKey(myPlay) == "paper") { 6 }
      else { 0 }
    } else if (playKey(enemyPlay) == "paper") {
      if (playKey(myPlay) == "scissors") { 6 }
      else { 0 }
    } else {
      if (playKey(myPlay) == "rock") { 6 }
      else { 0 }
    }
  }
}

def correctDecrypt(enemyPlay: String, myPlay: String): String = {
  // y is tie, x is lose, z is win
  if (myPlay == "y") {
    if (playKey(enemyPlay) == "rock") { "x" }
    else if (playKey(enemyPlay) == "paper") { "y" }
    else { "z" }
  } else {
    if (myPlay == "x") {
      if (playKey(enemyPlay) == "rock") { "z" }
      else if (playKey(enemyPlay) == "paper") { "x" }
      else { "y" }
    } else {
      if (playKey(enemyPlay) == "rock") { "y" }
      else if (playKey(enemyPlay) == "paper") { "z" }
      else { "x" }
    }
  }
}

def calcPoints(playList: List[String]): Int = {
  val totalScore = elfRPSKey.map { plays =>
    val play = plays.split("")
    val (enemyPlay, myPlay) = (play.head.toLowerCase, play.last.toLowerCase)
    resultsKey(myPlay) + calcRpsWinner(enemyPlay, myPlay)
  }
  totalScore.sum
}

def calcPoints2(playList: List[String]): Int = {
  val totalScore = elfRPSKey.map { plays =>
    val play = plays.split("")
    val (enemyPlay, myPlay) = (play.head.toLowerCase, play.last.toLowerCase)
    val newPlay = correctDecrypt(enemyPlay, myPlay)
    resultsKey(newPlay) + calcRpsWinner(enemyPlay, newPlay)
  }
  totalScore.sum
}

val totalPoints = calcPoints(elfRPSKey)
println(s"The points won are $totalPoints points")

val totalPoints2 = calcPoints2(elfRPSKey)
println(s"The points won are $totalPoints2 points\n")
