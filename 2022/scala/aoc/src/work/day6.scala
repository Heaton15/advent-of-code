package work

object day6 extends App {
  import scala.io.Source
  import scala.collection.mutable.ArrayStack
  import scala.collection.mutable.Stack
  import scala.collection.mutable.Queue
  import scala.collection.*

  val fileName = "inputs/elf_packetstream.txt"
  val dataStream = Source.fromFile(fileName).getLines.toList

  val input = Source.fromFile(fileName).getLines.mkString("")

  def allUnique[A](to: TraversableOnce[A]) = {
    val set = scala.collection.mutable.Set[A]()
    to.forall { x =>
      if (set(x)) false
      else {
        set += x
        true
      }
    }
  }

  def calcStartOfPacket(dataStream: List[String], msgSize: Int): Int = {
    var firstInts = Queue[Char]()
    var cnt = msgSize + 1
    val ls = dataStream.head.toList
    val newLs = ls.drop(msgSize)
    Seq.tabulate(msgSize)(_ + 0).foreach { firstInts enqueue ls(_) }

    def calcStream(q: Queue[Char], ls: List[Char]): Int = {
      q.enqueue(ls.head)
      q.dequeue

      if (allUnique(q)) { cnt }
      else {
        cnt += 1
        calcStream(q, ls.tail)
      }
    }

    calcStream(firstInts, newLs)
  }

  def betterStartOfPacket(dataStream: List[String], msgSize: Int): Int = {
    var cnt = msgSize
    val ls = dataStream.head.toList

    def calcStream(ls: List[Char], msgSize: Int): Int = {
      if (ls.slice(0, msgSize).toSet.size == msgSize) { cnt }
      else {
        cnt += 1
        calcStream(ls.tail, msgSize)
      }
    }
    calcStream(ls, msgSize)
  }

  val startOfPacket = calcStartOfPacket(dataStream, 4)
  println(s"Start of Packet: $startOfPacket\n")

  val startOfPacket2 = calcStartOfPacket(dataStream, 14)
  println(s"Start of Packet: $startOfPacket2\n")

  val startOfPacket3 = betterStartOfPacket(dataStream, 4)
  println(s"Start of Packet: $startOfPacket3\n")

  val startOfPacket4 = betterStartOfPacket(dataStream, 14)
  println(s"Start of Packet: $startOfPacket4\n")

  def scalaCenterSolution(input: String): Int = {
    val windows = input.sliding(4)
    def allDifferent(s: String): Boolean = s.toSet.size == 4
    val firstIndex = windows.indexWhere(allDifferent)
    firstIndex + 4
  }
}
