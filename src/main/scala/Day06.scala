import scala.io.Source

object Day06 {
  private val input = Source
    .fromResource("day06.txt")
    .mkString

  def runPart1(): Int = part1(input)

  def part1(input: String) = {
    val index = input
      .sliding(4, 1)
      .zipWithIndex
      .find((x, index) => x.toCharArray.toSet.size == 4)
      .get
      ._2

    index + 4
  }

  def runPart2(): Int = part2(input)

  def part2(input: String) = {
    val index = input
      .sliding(14, 1)
      .zipWithIndex
      .find((x, index) => x.toCharArray.toSet.size == 14)
      .get
      ._2

    index + 14
  }
}
