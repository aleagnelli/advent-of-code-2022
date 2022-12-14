class Day02Suite extends munit.FunSuite {
  private val example = """
      |A Y
      |B X
      |C Z
      """.stripMargin.trim

  test("part 1 example") {
    val actual = Day02.part1(example)
    assertEquals(actual, 15)
  }

  test("part 1 solution") {
    val actual = Day02.runPart1()
    assertEquals(actual, 13052)
  }

  test("part 2 example") {
    val actual = Day02.part2(example)
    assertEquals(actual, 12)
  }

  test("part 2 solution") {
    val actual = Day02.runPart2()
    assertEquals(actual, 13693)
  }
}
