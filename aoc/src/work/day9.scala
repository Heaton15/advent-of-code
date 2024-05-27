package work

import scala.io.Source

object day9 extends App {

  val fileName = "inputs/elf_ropes.txt"
  val data = Source.fromFile(fileName).getLines

  /* The puzzle input is comprised of commands that the head of the rope follows
   We then need to move the tail based on the head movement.

   the tail can:
   1. Be on top of the head
   2. Be diagonal of the head
   3. Must snap diagonally to be at least 1 space within the head

   To begin, we start at s or (0,0)
   H and T are on top of each other and this counts as a minimum point

   Possible Configurations:

   Over Lapping: H can move any direction and T will not move

   Column: L / R free for 1 space, U / D can create overlap or movement vertically

   Row: L / R can create overlap or movement horiontally, U / D is free for 1 space


   *** This seems to be the tricky one to track
   Diagonal: A permutation of L | R and U | D will either cause movement or cause no movement
    - The movement will cause the T location to take the old H position


    Note: We must track T locations. Let's do this in an immutable manner this time.


   x x x    (1, 1) -> H, (2, 0) -> T
   x H x
   x x T

   x H x    (2, 1) -> H, (2, 0) -> T , T copies H
   x T x
   x x x
   */

  enum Instruction {
    case L(d: Int)
    case R(d: Int)
    case U(d: Int)
    case D(d: Int)
  }

  enum PointRelationship {
    case Overlapping
    case HorizontalRight
    case HorizontalLeft
    case VerticalUp
    case VerticalDown
    case DiagonalLL
    case DiagonalUL
    case DiagonalUR
    case DiagonalLR
  }

  import Instruction.*
  import PointRelationship.*

  def getInstruction(s: String): Instruction = {
    s match {
      case s"L $d" => L(d.toInt)
      case s"R $d" => R(d.toInt)
      case s"U $d" => U(d.toInt)
      case s"D $d" => D(d.toInt)
    }
  }

  case class Point(val x: Int, val y: Int)

  class PointTracker(val h: Point, val t: Point, val pointMap: Map[Point, Int] = Map.empty[Point, Int]) {

    // The pointMap will be used as a dictionary to keep track of a point's
    // information. We don't need to link the cardinal directions of points,
    // just update the map with the point we are going to exist in and increment
    // if the T point goes through it.

    // Only utility functions on the PointTracker should be able to access the H / T
    // points in our problem.

    def report = {
      val s = s"""|(h,t): $h, $t""".stripMargin
      println(s)
    }

    // Let's us know how the T / H points are related
    private def pr: PointRelationship = {
      (h, t) match {
        case (Point(_, _), Point(_, _)) if (h.x == t.x) && (h.y == t.y) => Overlapping
        case (Point(_, _), Point(_, _)) if (h.y == t.y) && (h.x > t.x)  => HorizontalRight
        case (Point(_, _), Point(_, _)) if (h.y == t.y) && (h.x < t.x)  => HorizontalLeft
        case (Point(_, _), Point(_, _)) if (h.x == t.x) && (h.y > t.y)  => VerticalUp
        case (Point(_, _), Point(_, _)) if (h.x == t.x) && (h.y < t.y)  => VerticalDown
        case (Point(_, _), Point(_, _)) if (h.x > t.x) && (h.y > t.y)   => DiagonalUR
        case (Point(_, _), Point(_, _)) if (h.x > t.x) && (h.y < t.y)   => DiagonalLR
        case (Point(_, _), Point(_, _)) if (h.x < t.x) && (h.y < t.y)   => DiagonalLL
        case (Point(_, _), Point(_, _)) if (h.x < t.x) && (h.y > t.y)   => DiagonalUL
        case _ => throw new MatchError("Could not determine the T and H point relationship")
      }
    }

    // When T moves, we need to update the map
    private def updatePointMap(point: Point): Map[Point, Int] = {
      val maybePoint = pointMap.get(point)
      maybePoint match {
        case Some(i) => pointMap ++ Map(point -> (i + 1))
        case None    => pointMap ++ Map(point -> 0)
      }
    }

    private def up(p: Point) = Point(p.x, p.y + 1)
    private def down(p: Point) = Point(p.x, p.y - 1)
    private def left(p: Point) = Point(p.x - 1, p.y)
    private def right(p: Point) = Point(p.x + 1, p.y)

    /* Overlapping     -> move H, keep T
       HorizontalRight -> move H, keep T
       HorizontalLeft  -> move H, keep T
       VerticalDown    -> Move H, keep T
       DiagonalLL      -> Move H, keep T
       DiagonalLR      -> move H, keep T
       VerticalUp      -> Move H, move T UP
       DiagonalUL      -> move H, move T to old H
       DiagonalUR      -> move H, move T to old H
     */
    def goUp = {
      pr match {
        case Overlapping | HorizontalRight | HorizontalLeft | VerticalDown | DiagonalLL | DiagonalLR => new PointTracker(up(h), t, pointMap)
        case DiagonalUL | DiagonalUR => new PointTracker(up(h), h, updatePointMap(h))
        case VerticalUp              => new PointTracker(up(h), up(t), updatePointMap(up(t)))
      }
    }

    /* Overlapping     -> move H, keep T
       HorizontalRight -> move H, keep T
       HorizontalLeft  -> move H, keep T
       VerticalUp      -> Move H, keep T
       DiagonalUL      -> move H, keep T
       DiagonalUR      -> move H, keep T
       DiagonalLL      -> Move H, move T to old H
       DiagonalLR      -> move H, move T to old H
       VerticalDown    -> Move H, move T
     */
    def goDown = {
      pr match {
        case Overlapping | HorizontalRight | HorizontalLeft | VerticalUp | DiagonalUL | DiagonalUR => new PointTracker(down(h), t, pointMap)
        case DiagonalLL | DiagonalLR => new PointTracker(down(h), h, updatePointMap(h))
        case VerticalDown            => new PointTracker(down(h), down(t), updatePointMap(down(t)))
      }
    }

    /* Overlapping     -> move H, keep T
       HorizontalRight -> move H, keep T
       VerticalUp      -> Move H, keep T
       DiagonalUR      -> move H, keep T
       VerticalDown    -> Move H, keep T
       DiagonalLR      -> move H, keep T
       DiagonalUL      -> move H, move T to old H
       DiagonalLL      -> Move H, move T to old H
       HorizontalLeft  -> move H, move T
     */
    def goLeft = {
      pr match {
        case Overlapping | HorizontalRight | VerticalUp | DiagonalUR | VerticalDown | DiagonalLR => new PointTracker(left(h), t, pointMap)
        case DiagonalUL | DiagonalLL => new PointTracker(left(h), h, updatePointMap(h))
        case HorizontalLeft          => new PointTracker(left(h), left(t), updatePointMap(left(t)))
      }
    }

    /* Overlapping     -> move H, keep T
       DiagonalUL      -> move H, keep T
       DiagonalLL      -> Move H, keep T
       HorizontalLeft  -> move H, keep T
       VerticalUp      -> Move H, keep T
       VerticalDown    -> Move H, keep T
       DiagonalUR      -> move H, move T to old H
       DiagonalLR      -> move H, move T to old H
       HorizontalRight -> move H, move T
     */
    def goRight = {
      pr match {
        case Overlapping | DiagonalUL | DiagonalLL | HorizontalLeft | VerticalUp | VerticalDown => new PointTracker(right(h), t, pointMap)
        case DiagonalUR | DiagonalLR => new PointTracker(right(h), h, updatePointMap(h))
        case HorizontalRight         => new PointTracker(right(h), right(t), updatePointMap(right(t)))
      }
    }
  }

  // We need to start on the origin with an unbounded growing location
  val instr: List[Instruction] = data.toList.map(i => getInstruction(i))
  val tracker = PointTracker(Point(0, 0), Point(0, 0), Map(Point(0, 0) -> 1))

  def processInstr(instr: List[Instruction], tracker: PointTracker): PointTracker = {
    val i = instr.head

    val newTracker = i match {
      case L(l) => List.tabulate(l)(_ + 0).foldLeft(tracker)((tr, i) => tr.goLeft)
      case R(r) => List.tabulate(r)(_ + 0).foldLeft(tracker)((tr, i) => tr.goRight)
      case U(u) => List.tabulate(u)(_ + 0).foldLeft(tracker)((tr, i) => tr.goUp)
      case D(d) => List.tabulate(d)(_ + 0).foldLeft(tracker)((tr, i) => tr.goDown)
    }
    //newTracker.report
    if (instr.tail.isEmpty) newTracker
    else processInstr(instr.tail, newTracker)
  }

  val solution_1 = processInstr(instr, tracker).pointMap.values.filter(_ >= 1).sum
  println(solution_1)

}
