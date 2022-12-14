class Day09Suite extends munit.FunSuite {
  private val example = """
      |R 4
      |U 4
      |L 3
      |D 1
      |R 4
      |D 1
      |L 5
      |R 2
      """.stripMargin.trim

  private val largeExample = """
      |R 5
      |U 8
      |L 8
      |D 3
      |R 17
      |D 10
      |L 25
      |U 20
      """.stripMargin.trim

  test("part 1 example") {
    val actual = Day09.part1(example)
    assertEquals(actual, 13)
  }

  test("part 1 solution") {
    val actual = Day09.runPart1()
    assertEquals(actual, 6044)
  }

  test("part 2 example") {
    val actual = Day09.part2(example)
    assertEquals(actual, 1)
    println()
    val actualLarge = Day09.part2(largeExample)
    assertEquals(actualLarge, 36)
  }

  test("part 2 solution") {
    val actual = Day09.runPart2()
    assertEquals(actual, 2384)
  }
}
