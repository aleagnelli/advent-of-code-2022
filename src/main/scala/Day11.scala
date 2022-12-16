import scala.io.Source

object Day11 {
  private case class Monkey(
      name: String,
      items: List[Long],
      worryLevelChanger: Long => Long,
      testSpec: TestSpec,
      inspectedItems: Long = 0
  ) {
    def addItem(item: Long) = copy(items = items.appended(item))

    def emptyItems = copy(items = List.empty)

    def increaseIfInspected = copy(inspectedItems = inspectedItems + items.length)
  }

  private case class TestSpec(divider: Long, actionTrue: String, actionFalse: String) {
    def run(lvl: Long) = if lvl % divider == 0 then actionTrue else actionFalse
  }
  private val input = Source
    .fromResource("day11.txt")
    .mkString

  def runPart1(): Long = part1(input)

  def part1(input: String) = {
    val monkeys = parseMonkeys(input)
    playRounds(monkeys, 20, None).values
      .map(_.inspectedItems)
      .toList
      .sorted
      .reverse
      .take(2)
      .reduce(_ * _)
  }

  def runPart2(): Long = part2(input)

  def part2(input: String) = {
    val monkeys = parseMonkeys(input)
    val worryManager = monkeys.values.map(_.testSpec.divider).reduce(_ * _)
    val finalMonkeyPlay = playRounds(monkeys, 10000, Option(worryManager))
    finalMonkeyPlay.values
      .map(_.inspectedItems)
      .toList
      .sorted
      .reverse
      .take(2)
      .reduce(_ * _)
  }

  private def parseMonkeys(monkeysRaw: String) =
    monkeysRaw
      .split("\n\n")
      .map(parseMonkey)
      .toMap

  private def parseMonkey(monkeyRaw: String) = {
    val Array(monkeyName, body) = monkeyRaw.split(":\n")
    val monkey = monkeyRaw.split(" ").drop(1).head.replace(":", "").trim
    val Array(startingItemsRaw, operationRaw, testRaw, actionTrueRaw, actionFalseRaw) =
      body.split("\n").map(_.trim)
    val startingItems =
      startingItemsRaw
        .replace("Starting items: ", "")
        .split(",")
        .map(_.trim.toLong)
        .toList
    val operation = operationRaw.replace("Operation: ", "") match {
      case s"new = old $op old" =>
        (old: Long) =>
          op match
            case "*" => old * old

      case s"new = old $op $amount" =>
        (old: Long) =>
          op match
            case "*" => old * amount.toLong
            case "+" => old + amount.toLong
    }
    val condition = testRaw.replace("Test: divisible by ", "")
    val actionTrue = actionTrueRaw.replace("If true: throw to monkey ", "")
    val actionFalse = actionFalseRaw.replace("If false: throw to monkey ", "")
    val testSpec = TestSpec(condition.toLong, actionTrue, actionFalse)
    (monkey, Monkey(monkey, startingItems, operation, testSpec))
  }

  private def playRounds(
      monkeys: Map[String, Monkey],
      rounds: Int,
      relief: Option[Long]
  ): Map[String, Monkey] =
    (1 to rounds).foldLeft(monkeys) { (prevRounds, _) => playRound(prevRounds, relief) }

  private def playRound(
      monkeys: Map[String, Monkey],
      relief: Option[Long] = None
  ): Map[String, Monkey] = {
    monkeys.keySet.toList.sorted
      .foldLeft(monkeys) { (monkeys, monkeyName) =>
        val monkey = monkeys(monkeyName)
        val inspectItems = monkey.items
        inspectItems
          .map(item => {
            val worryLevelIncreased = monkey.worryLevelChanger(item)
            val newWorryLevel = relief
              .map(mod => worryLevelIncreased % mod)
              .getOrElse(worryLevelIncreased / 3)
            val newMonkey = monkey.testSpec.run(newWorryLevel)
            (newWorryLevel, newMonkey)
          })
          .foldLeft(monkeys) { case (monkeys, (lvl, monkeyTo)) =>
            monkeys.updatedWith(monkeyTo) {
              case None => throw new IllegalStateException("This should never supposed to happen")
              case Some(m) => Option(m.addItem(lvl))
            }
          }
          .updatedWith(monkeyName) {
            case None    => throw new IllegalStateException("This should never supposed to happen")
            case Some(m) => Option(m.increaseIfInspected.emptyItems)
          }
      }
  }
}
