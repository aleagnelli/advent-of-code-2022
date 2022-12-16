import scala.io.Source

object Day10 {
  private val input = Source
    .fromResource("day10.txt")
    .mkString

  def runPart1(): Int = part1(input)

  def part1(input: String) = {
    val (signalStrengths, _, _) = input.linesIterator
      .foldLeft((0, 1, 1)) { case ((signalStrengths, cycle, reg), instruction) =>
        instruction match {
          case "noop" =>
            val additionalStrength = interestingSignalStrength(cycle, reg)
            (signalStrengths + additionalStrength, cycle + 1, reg)
          case s"addx $amount" =>
            val additionalStrength =
              interestingSignalStrength(cycle, reg) + interestingSignalStrength(cycle + 1, reg)
            (signalStrengths + additionalStrength, cycle + 2, reg + amount.toInt)
        }
      }
    signalStrengths
  }

  private def interestingSignalStrength(cycle: Int, reg: Int) =
    if isCycleInteresting(cycle) then reg * cycle else 0

  private def isCycleInteresting(cycle: Int) = (cycle - 20) % 40 == 0

  def runPart2(): String = part2(input)

  def part2(input: String) = {
    val (crt, _, _) = input.linesIterator
      .foldLeft((List.empty[Char], 1, 1)) { case ((crt, cycle, spritePos), instruction) =>
        instruction match {
          case "noop" =>
            val crtUpdated = drawPixel(cycle, spritePos) +: crt
            (crtUpdated, cycle + 1, spritePos)
          case s"addx $amount" =>
            val newSpritePos = spritePos + amount.toInt
            val crtUpdated = drawPixel(cycle + 1, spritePos) +: drawPixel(cycle, spritePos) +: crt
            (crtUpdated, cycle + 2, newSpritePos)
        }
      }

    crt.reverse.sliding(40, 40).map(_.mkString).mkString("\n")
  }

  private def drawPixel(cycle: Int, spritePos: Int): Char =
    if ((spritePos - 1 to spritePos + 1).contains((cycle - 1) % 40)) '#' else '.'
}
