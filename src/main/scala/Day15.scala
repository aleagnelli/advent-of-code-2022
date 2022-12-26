import scala.io.Source
import scala.annotation.tailrec

object Day15 {

  private case class Range(from: Int, to: Int)

  private case class Coordinates(x: Int, y: Int):
    def distance(other: Coordinates) = math.abs(x - other.x) + math.abs(y - other.y)
    def inRange(r: Int, row: Int): Option[Range] =
      if y - r <= row && row <= y + r then
        val d2Row = math.abs(this.y - row)
        Option(Range(x - r + d2Row, x + r - d2Row))
      else None

  opaque type Sensor = Coordinates
  opaque type Beacon = Coordinates

  private val input = Source
    .fromResource("day15.txt")
    .mkString

  def runPart1(): Int = part1(input, 2000000)

  def part1(input: String, row: Int) =
    val report = parseReport(input)
    val ranges = report.flatMap { case (s, b) =>
      val d = s.distance(b)
      s.inRange(d, row)
    }
    merge(ranges).map(r => math.abs(r.from - r.to)).sum

  def runPart2(): Long = part2(input, 4000000)

  def part2(input: String, row: Int) =
    val report = parseReport(input)
    val (r, coverage) = (0 to row)
      .map { y =>
        val coverage = merge(
          report
            .flatMap { case (s, b) =>
              val d = s.distance(b)
              s.inRange(d, y)
            }
        )
        (y, coverage)
      }
      .filter(_._2.size > 1)
      .head
    val x = coverage.sortBy(_.from).head.to + 1
    x.toLong * 4000000L + r
  end part2

  private def parseReport(raw: String) =
    raw.split("\n").map(parseSensor).toList

  private def parseSensor(sensor: String): (Sensor, Beacon) =
    sensor match
      case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
        (Coordinates(sx.toInt, sy.toInt), Coordinates(bx.toInt, by.toInt))

  private def merge(ranges: List[Range]): List[Range] =
    @tailrec
    def go(rem: List[Range], acc: List[Range]): List[Range] =
      rem match
        case Nil       => acc
        case r1 :: Nil => r1 :: acc
        case r1 :: r2 :: r3 =>
          (r1, r2) match
            case (Range(f, t), Range(f2, t2)) if f2 <= t + 1 && t <= t2 =>
              go(Range(f, t2) :: r3, acc)
            case (Range(f, t), Range(f2, t2)) if f2 <= t + 1 && t2 <= t =>
              go(Range(f, t) :: r3, acc)
            case (_, _) => go(r2 :: r3, r1 :: acc)

    val r = ranges.sortBy(_.from)
    go(r, List.empty)
  end merge

}
