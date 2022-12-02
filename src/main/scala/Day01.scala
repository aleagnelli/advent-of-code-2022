import scala.io.Source

object Day01 {
  private val input = Source
    .fromResource("day01.txt")
    .mkString
    .split("\n\n")
    .map(group => group.split("\n").map(c => c.toInt).toList)
    .toList

  type ElfCalories = List[Int]

  def runPart1(): Int = part1(input)

  def part1(elves: List[ElfCalories]) = elves.map(elf => elf.sum).max

  def runPart2(): Int = part2(input)

  def part2(elves: List[ElfCalories]) =
    elves.map(elf => elf.sum).sorted.reverse.take(3).sum

}
