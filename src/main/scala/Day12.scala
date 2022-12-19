import scala.io.Source

object Day12 {
  private case class Point(x: Int, y: Int) {
    def steps = List(
      Point(x + 1, y),
      Point(x - 1, y),
      Point(x, y + 1),
      Point(x, y - 1)
    )
  }

  opaque type Cost = Int
  opaque type Height = Int

  private val input = Source
    .fromResource("day12.txt")
    .mkString

  def runPart1(): Int = part1(input)

  def part1(input: String) = {
    val (heights, start, _, end) = parseInput(input)
    val costs = walkCosts(heights, start)
    costs(end).toInt
  }

  def runPart2(): Int = part2(input)

  def part2(input: String) = {
    val (heights, _, lowests, end) = parseInput(input)
    lowests.map { start =>
      val costs = walkCosts(heights, start)
      costs(end).toInt
    }.min
  }

  private def parseInput(raw: String) = {
    val matrixChar = raw.linesIterator.toList.map(_.toCharArray().toList)
    val heights = matrixChar.zipWithIndex
      .map { case (row, y) =>
        row.zipWithIndex.map { case (c, x) =>
          val height = c match
            case 'S'     => 'a'.toInt
            case 'E'     => 'z'.toInt
            case c: Char => c.toInt
          (Point(x, y), height)
        }
      }
      .flatten
      .toMap
    val start = getPositionsOf(matrixChar, 'S').head
    val end = getPositionsOf(matrixChar, 'E').head
    val lowests = getPositionsOf(matrixChar, 'a')
    (heights, start, start +: lowests, end)
  }

  private def getPositionsOf(matrix: List[List[Char]], what: Char) =
    matrix.zipWithIndex
      .map { case (row, y) =>
        row.zipWithIndex
          .find { case (c, x) => c == what }
          .map { case (_, x) => Point(x, y) }
      }
      .filter(_.isDefined)
      .map(_.get)

  private def walkCosts(heights: Map[Point, Height], start: Point) = {
    def walkStep(
        nextStep: List[Point],
        costs: Map[Point, Cost],
        visited: Set[Point]
    ): Map[Point, Cost] = {
      nextStep match
        case Nil => costs
        case currentPoint :: next if visited.contains(currentPoint) =>
          walkStep(next, costs, visited)
        case currentPoint :: next =>
          val currentHeight = heights(currentPoint)
          val currentCost = costs(currentPoint)
          val pointsFromHere = currentPoint.steps
            .filter(p => heights.get(p).map(height => height <= currentHeight + 1).getOrElse(false))
          val costsUpdated = pointsFromHere.foldLeft(costs) { case (costs, p) =>
            costs.updatedWith(p) {
              case None          => Some(currentCost + 1)
              case Some(oldCost) => Some(oldCost min (currentCost + 1))
            }
          }

          walkStep(next ++ pointsFromHere, costsUpdated, visited + currentPoint)
    }

    walkStep(List(start), Map(start -> 0), Set.empty)
  }
}
