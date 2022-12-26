import scala.annotation.tailrec
import scala.io.Source

object Day14 {

  private case class Coordinates(x: Int, y: Int) {
    def until(e: Coordinates): Set[Coordinates] =
      (for
        x <- math.min(this.x, e.x) to math.max(this.x, e.x)
        y <- math.min(this.y, e.y) to math.max(this.y, e.y)
      yield Coordinates(x, y)).toSet

    def down = copy(y = y + 1)
    def leftDown = copy(x = x - 1, y = y + 1)
    def rightDown = copy(x = x + 1, y = y + 1)
  }

  private object Coordinates {
    def parse(raw: String) =
      val Array(x, y) = raw.split(",").map(_.toInt)
      Coordinates(x, y)
  }

  private val input = Source
    .fromResource("day14.txt")
    .mkString

  def runPart1(): Int = part1(input)

  def part1(input: String) =
    val rocks = parseRocksPositions(input)
    analyzeSand(rocks)

  def runPart2(): Int = part2(input)

  def part2(input: String) =
    val rocks = parseRocksPositions(input)
    analyzeSand2(rocks)

  private def parseRocksPositions(raw: String) =
    raw.linesIterator.flatMap { path =>
      path
        .split(" -> ")
        .map(Coordinates.parse)
        .sliding(2)
        .flatMap { case Array(s, e) => s.until(e) }
        .toSet
    }.toSet

  private def analyzeSand(rocks: Set[Coordinates]) =
    val lowestOfThemAll = rocks.maxBy(_.y).y
    val sandRested: Set[Coordinates] = simulateSand(rocks, Set.empty, lowestOfThemAll)
    sandRested.size

  @tailrec
  private def simulateSand(
      rocks: Set[Coordinates],
      sand: Set[Coordinates],
      lastStepBeforeAbyss: Int
  ): Set[Coordinates] =
    simulateFallingUnit(rocks ++ sand, lastStepBeforeAbyss) match
      case None    => sand
      case Some(s) => simulateSand(rocks, sand + s, lastStepBeforeAbyss)

  private def simulateFallingUnit(pattern: Set[Coordinates], lastStepBeforeAbyss: Int) =
    @tailrec
    def go(current: Coordinates): Option[Coordinates] =
      if current.y > lastStepBeforeAbyss then None
      else if pattern.contains(current.down) then
        if !pattern.contains(current.leftDown) then go(current.leftDown)
        else if !pattern.contains(current.rightDown) then go(current.rightDown)
        else Option(current)
      else go(current.down)
    go(Coordinates(500, 0))

  private def analyzeSand2(rocks: Set[Coordinates]) =
    val lowestOfThemAll = rocks.maxBy(_.y).y
    val sandRested: Set[Coordinates] = simulateSand2(rocks, Set.empty, lowestOfThemAll + 2)
    sandRested.size

  @tailrec
  private def simulateSand2(
      rocks: Set[Coordinates],
      sand: Set[Coordinates],
      floorLevel: Int
  ): Set[Coordinates] =
    simulateFallingUnit2(rocks ++ sand, floorLevel) match
      case s @ Coordinates(500, 0) => sand + s
      case s                       => simulateSand2(rocks, sand + s, floorLevel)

  private def simulateFallingUnit2(pattern: Set[Coordinates], floorLevel: Int) =
    @tailrec
    def go(current: Coordinates): Coordinates =
      if current.down.y == floorLevel then current
      else if pattern.contains(current.down) then
        if !pattern.contains(current.leftDown) then go(current.leftDown)
        else if !pattern.contains(current.rightDown) then go(current.rightDown)
        else current
      else go(current.down)
    go(Coordinates(500, 0))

}
