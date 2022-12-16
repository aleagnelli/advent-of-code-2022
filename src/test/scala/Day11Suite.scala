class Day11Suite extends munit.FunSuite {
  private val example = """
  |Monkey 0:
  |  Starting items: 79, 98
  |  Operation: new = old * 19
  |  Test: divisible by 23
  |    If true: throw to monkey 2
  |    If false: throw to monkey 3
  |
  |Monkey 1:
  |  Starting items: 54, 65, 75, 74
  |  Operation: new = old + 6
  |  Test: divisible by 19
  |    If true: throw to monkey 2
  |    If false: throw to monkey 0
  |
  |Monkey 2:
  |  Starting items: 79, 60, 97
  |  Operation: new = old * old
  |  Test: divisible by 13
  |    If true: throw to monkey 1
  |    If false: throw to monkey 3
  |
  |Monkey 3:
  |  Starting items: 74
  |  Operation: new = old + 3
  |  Test: divisible by 17
  |    If true: throw to monkey 0
  |    If false: throw to monkey 1
  """.stripMargin.trim

  test("part 1 example") {
    val actual = Day11.part1(example)
    assertEquals(actual, 10605L)
  }

  test("part 1 solution") {
    val actual = Day11.runPart1()
    assertEquals(actual, 58322L)
  }

  test("part 2 example") {
    val actual = Day11.part2(example)
    assertEquals(actual, 2713310158L)
  }

  test("part 2 solution") {
    val actual = Day11.runPart2()
    assertEquals(actual, 13937702909L)
  }
}
