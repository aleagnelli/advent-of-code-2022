class Day08Suite extends munit.FunSuite {
  private val example = """
      |30373
      |25512
      |65332
      |33549
      |35390
      """.stripMargin.trim

  test("part 1 example") {
    val actual = Day08.part1(example)
    assertEquals(actual, 21)
  }

  test("part 1 solution") {
    val actual = Day08.runPart1()
    assertEquals(actual, 1711)
  }

  test("part 2 example") {
    val actual = Day08.part2(example)
    assertEquals(actual, 8)
  }

  test("part 2 solution") {
    val actual = Day08.runPart2()
    assertEquals(actual, 301392)
  }
}
