class Day14Suite extends munit.FunSuite {
  private val example = """
      |498,4 -> 498,6 -> 496,6
      |503,4 -> 502,4 -> 502,9 -> 494,9
      """.stripMargin.trim

  test("part 1 example") {
    val actual = Day14.part1(example)
    assertEquals(actual, 24)
  }

  test("part 1 solution") {
    val actual = Day14.runPart1()
    assertEquals(actual, 1133)
  }

  test("part 2 example") {
    val actual = Day14.part2(example)
    assertEquals(actual, 93)
  }

  test("part 2 solution") {
    val actual = Day14.runPart2()
    assertEquals(actual, 27566)
  }
}
