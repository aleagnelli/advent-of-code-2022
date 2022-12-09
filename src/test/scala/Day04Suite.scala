class Day04Suite extends munit.FunSuite {
  private val example = """
      |2-4,6-8
      |2-3,4-5
      |5-7,7-9
      |2-8,3-7
      |6-6,4-6
      |2-6,4-8
      """.stripMargin.trim

  test("part 1 example") {
    val actual = Day04.part1(example)
    assertEquals(actual, 2)
  }

  test("part 1 solution") {
    val actual = Day04.runPart1()
    assertEquals(actual, 475)
  }

  test("part 2 example") {
    val actual = Day04.part2(example)
    assertEquals(actual, 4)
  }

  test("part 2 solution") {
    val actual = Day04.runPart2()
    assertEquals(actual, 825)
  }
}
