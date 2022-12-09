import scala.io.Source

object Day04 {
  private val input = Source
    .fromResource("day04.txt")
    .mkString

  private case class Sections(from: Int, to: Int) {
    def contains(other: Sections): Boolean =
      this.from <= other.from && other.to <= this.to

    def overlap(other: Sections): Boolean = {
      val isOverlapLeft = this.from <= other.from && other.from <= this.to && this.to <= other.to
      val isOverlapRight = other.from <= this.from && this.from <= other.to && other.to <= this.to
      isOverlapLeft || isOverlapRight || this.contains(other) || other.contains(this)
    }
  }

  def runPart1(): Int = part1(input)

  def part1(input: String) = input.linesIterator.toList.count(pair => {
    val assignementsSections = pair
      .split(",")
      .map(assignements => {
        val bounds = assignements.split("-").map(_.toInt)
        Sections(bounds(0), bounds(1))
      })
    val firstElfSections = assignementsSections(0)
    val secondElfSections = assignementsSections(1)
    firstElfSections.contains(secondElfSections) || secondElfSections.contains(firstElfSections)
  })

  def runPart2(): Int = part2(input)

  def part2(input: String) = input.linesIterator.toList.count(pair => {
    val assignementsSections = pair
      .split(",")
      .map(assignements => {
        val bounds = assignements.split("-").map(_.toInt)
        Sections(bounds(0), bounds(1))
      })
    val firstElfSections = assignementsSections(0)
    val secondElfSections = assignementsSections(1)
    firstElfSections.overlap(secondElfSections)
  })

}
