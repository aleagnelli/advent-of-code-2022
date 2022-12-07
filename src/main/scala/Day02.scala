import scala.io.Source

object Day02 {
  enum Shape(val points: Int) {
    case Rock extends Shape(1)
    case Paper extends Shape(2)
    case Scissor extends Shape(3)
  }

  object Shape {
    def parse(s: String): Shape = s.trim match {
      case "X" => Rock
      case "Y" => Paper
      case "Z" => Scissor
      case "A" => Rock
      case "B" => Paper
      case "C" => Scissor
    }

    def counter(shape: Shape, outcome: GameOutcome): Shape = (shape, outcome) match {
      case (shape, GameOutcome.Draw)   => shape
      case (Rock, GameOutcome.Win)     => Paper
      case (Rock, GameOutcome.Lost)    => Scissor
      case (Paper, GameOutcome.Win)    => Scissor
      case (Paper, GameOutcome.Lost)   => Rock
      case (Scissor, GameOutcome.Win)  => Rock
      case (Scissor, GameOutcome.Lost) => Paper
    }
  }

  enum GameOutcome(val points: Int) {
    case Lost extends GameOutcome(0)
    case Draw extends GameOutcome(3)
    case Win extends GameOutcome(6)
  }

  object GameOutcome {
    def parse(s: String): GameOutcome = {
      s.trim match {
        case "X" => Lost
        case "Y" => Draw
        case "Z" => Win
      }
    }
  }

  private val input = Source
    .fromResource("day02.txt")
    .mkString

  def runPart1(): Int = part1(input)

  def part1(strategyRaw: String) = {
    val game = parse1(strategyRaw)

    game.foldLeft(0) {
      case (res, game) => {
        val outcome = play(game)
        res + outcome.points + game._2.points
      }
    }
  }

  private def play(game: (Shape, Shape)) = game match {
    case (Shape.Rock, Shape.Rock)       => GameOutcome.Draw
    case (Shape.Paper, Shape.Paper)     => GameOutcome.Draw
    case (Shape.Scissor, Shape.Scissor) => GameOutcome.Draw
    case (Shape.Rock, Shape.Paper)      => GameOutcome.Win
    case (Shape.Paper, Shape.Scissor)   => GameOutcome.Win
    case (Shape.Scissor, Shape.Rock)    => GameOutcome.Win
    case (Shape.Rock, Shape.Scissor)    => GameOutcome.Lost
    case (Shape.Paper, Shape.Rock)      => GameOutcome.Lost
    case (Shape.Scissor, Shape.Paper)   => GameOutcome.Lost
  }

  def runPart2(): Int = part2(input)

  def part2(strategyRaw: String) = {
    val game = parse2(strategyRaw)

    game.foldLeft(0) {
      case (res, (shape, outcome)) => {
        val counterShape = Shape.counter(shape, outcome)
        res + outcome.points + counterShape.points
      }
    }
  }

  private def parse1(raw: String) = {
    raw.linesIterator.toList
      .map(s => {
        val (o, y) = s.splitAt(1)
        (Shape.parse(o), Shape.parse(y))
      })
  }

  private def parse2(raw: String) = {
    raw.linesIterator.toList
      .map(s => {
        val (o, y) = s.splitAt(1)
        (Shape.parse(o), GameOutcome.parse(y))
      })
  }

}
