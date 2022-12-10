import scala.io.Source

object Day05 {

  private val input = Source
    .fromResource("day05.txt")
    .mkString

  type Mark = Char
  type Stack = List[Mark]

  case class ProcedureStep(val quantity: Int, val from: Int, val to: Int)

  def runPart1(): String = part1(input)

  def part1(input: String) = {
    val (startingPos, procedure) = parseInput(input)

    val finalCargo = procedure.foldLeft(startingPos) { (cargo, step) =>
      move(cargo, step.from, step.to, step.quantity)
    }
    finalCargo.keys.toList.sorted.map(n => finalCargo(n).head).mkString
  }

  private def move(cargo: Map[Int, Stack], from: Int, to: Int, quantity: Int): Map[Int, Stack] = {
    if (quantity == 0) {
      cargo
    } else {
      val crateToMove = cargo(from).head
      val newCargo = cargo
        .updatedWith(to) {
          case None        => Option(List(crateToMove))
          case Some(stack) => Option(crateToMove +: stack)
        }
        .updatedWith(from) {
          case None        => None
          case Some(stack) => Option(stack.drop(1))
        }
      move(newCargo, from, to, quantity - 1)
    }
  }

  def runPart2(): String = part2(input)

  def part2(input: String) = {
    val (startingPos, procedure) = parseInput(input)

    val finalCargo = procedure.foldLeft(startingPos) { (cargo, step) =>
      moveCrate9001(cargo, step.from, step.to, step.quantity)
    }
    finalCargo.keys.toList.sorted.map(n => finalCargo(n).head).mkString
  }

  private def moveCrate9001(
      cargo: Map[Int, Stack],
      from: Int,
      to: Int,
      quantity: Int
  ): Map[Int, Stack] = {
    val cratesToMove = cargo(from).take(quantity)
    cargo
      .updatedWith(to) {
        case None        => Option(cratesToMove)
        case Some(stack) => Option(cratesToMove ++ stack)
      }
      .updatedWith(from) {
        case None        => None
        case Some(stack) => Option(stack.drop(quantity))
      }
  }

  private def parseInput(input: String) = {
    val splitted = input.split("\n\n")
    val startingPos = parseStartingPosition(splitted(0))
    val procedure = parseRearrangementProcedure(splitted(1))
    (startingPos, procedure)
  }

  private def parseStartingPosition(input: String) = {
    val layers = input.split("\n").toList.reverse.drop(1)
    layers.foldLeft(Map[Int, Stack]()) { (cargo, layer) =>
      layer
        .sliding(3, 4)
        .zipWithIndex
        .foldLeft(cargo) { case (cargo, (crate, crateNumber)) =>
          if (crate.trim().isEmpty()) {
            cargo
          } else {
            val mark = crate(1)
            cargo.updatedWith(crateNumber + 1) {
              case None        => Option(List(mark))
              case Some(stack) => Option(mark +: stack)
            }
          }
        }
    }
  }

  private def parseRearrangementProcedure(input: String) = {
    input
      .split("\n")
      .map(step =>
        step
          .replaceAllLiterally("move ", "")
          .replaceAllLiterally("from ", "")
          .replaceAllLiterally("to ", "")
          .trim()
      )
      .map(step => {
        val parsed = step.split(" ").map(_.toInt)
        ProcedureStep(parsed(0), parsed(1), parsed(2))
      })
  }
}
