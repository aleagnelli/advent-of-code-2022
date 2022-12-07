import scala.io.Source

object Day03 {
  private val input = Source
    .fromResource("day03.txt")
    .mkString

  def runPart1(): Int = part1(input)

  def part1(s: String) =
    s.linesIterator
      .map(rucksack => {
        val (f, s) = rucksack.splitAt(rucksack.length() / 2)
        val firstComp = f.toCharArray().toSet
        val secondComp = s.toCharArray().toSet
        val itemToRearrange = firstComp.intersect(secondComp).head
        priority(itemToRearrange)
      })
      .sum

  def runPart2(): Int = part2(input)

  def part2(s: String) = s.linesIterator
    .grouped(3)
    .map(group => {
      val groupBadge = group
        .map(rucksack => rucksack.toCharArray().toSet)
        .reduce(_.intersect(_))
        .head
      priority(groupBadge)
    })
    .sum

  private def priority(itemType: Char): Int =
    if (itemType.isUpper) {
      itemType.toInt - 64 + 26
    } else {
      itemType.toInt - 96
    }
}
