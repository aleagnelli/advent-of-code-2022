import scala.io.Source

object Day09 {
  private case class Coordinates(x: Int, y: Int) {
    def moveX(amount: Int) = this.copy(x = x + amount)
    def moveY(amount: Int) = this.copy(y = y + amount)
  }

  private object Coordinates {
    def start = Coordinates(0, 0)
  }

  private case class Rope(head: Coordinates, knots: List[Coordinates]) {
    def right = newRope(head.moveX(1))
    def left = newRope(head.moveX(-1))
    def up = newRope(head.moveY(1))
    def down = newRope(head.moveY(-1))

    def tail = knots.last

    private def newRope(head: Coordinates) = {
      val (_, newKnots) = knots.foldLeft((head, List.empty[Coordinates])) {
        case ((prevKnot, newKnots), knot) =>
          val newKnot = follow(prevKnot, knot)
          (newKnot, newKnot +: newKnots)
      }
      Rope(head, newKnots.reverse)
    }

    private def follow(previousKnot: Coordinates, knot: Coordinates) = {
      val xDistanceAbs = math.abs(previousKnot.x - knot.x)
      val yDistanceAbs = math.abs(previousKnot.y - knot.y)
      val xMovement = math.signum(previousKnot.x - knot.x)
      val yMovement = math.signum(previousKnot.y - knot.y)

      if (xDistanceAbs <= 1 && yDistanceAbs <= 1) {
        knot
      } else {
        knot.moveX(xMovement).moveY(yMovement)
      }
    }
  }

  private object Rope {
    def start(n: Int) = Rope(Coordinates.start, (1 until n).map(_ => Coordinates.start).toList)
  }

  private val input = Source
    .fromResource("day09.txt")
    .mkString

  def runPart1(): Int = part1(input)

  def part1(input: String) = {
    val cmds = input.split("\n")
    val (_, visited) = cmds.foldLeft((Rope.start(2), Set[Coordinates]())) {
      case ((rope, visitedTails), cmd) =>
        val Array(direction, amount) = cmd.split(" ")
        val (newRope, ropes) = (1 to amount.toInt).foldLeft((rope, List.empty[Rope])) {
          case ((rope, ropes), _) =>
            val newRope = direction match {
              case "R" => rope.right
              case "L" => rope.left
              case "U" => rope.up
              case "D" => rope.down
            }
            (newRope, newRope +: ropes)
        }
        (newRope, visitedTails ++ ropes.map(_.tail).toSet)
    }
    visited.size
  }

  def runPart2(): Int = part2(input)

  def part2(input: String) = {
    val cmds = input.split("\n")
    val (_, visited) = cmds.foldLeft((Rope.start(10), Set[Coordinates]())) {
      case ((rope, visitedTails), cmd) =>
        val Array(direction, amount) = cmd.split(" ")
        val (newRope, ropes) = (1 to amount.toInt).foldLeft((rope, List.empty[Rope])) {
          case ((rope, ropes), _) =>
            val newRope = direction match {
              case "R" => rope.right
              case "L" => rope.left
              case "U" => rope.up
              case "D" => rope.down
            }
            (newRope, newRope +: ropes)
        }
        (newRope, visitedTails ++ ropes.map(_.tail).toSet)
    }
    visited.size
  }
}
