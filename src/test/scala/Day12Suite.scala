class Day12Suite extends munit.FunSuite {
  private val example = """
      |Sabqponm
      |abcryxxl
      |accszExk
      |acctuvwj
      |abdefghi
      """.stripMargin.trim

  test("part 1 example") {
    val actual = Day12.part1(example)
    assertEquals(actual, 31)
  }

  test("part 1 solution") {
    val actual = Day12.runPart1()
    assertEquals(actual, 350)
  }

  test("part 2 example") {
    val actual = Day12.part2(example)
    assertEquals(actual, 29)
  }

  test("part 2 solution") {
    val actual = Day12.runPart2()
    assertEquals(actual, 349)
  }
}
