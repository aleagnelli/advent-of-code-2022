class Day01Suite extends munit.FunSuite {
  private val example = List(
    List(1000, 2000, 3000),
    List(4000),
    List(5000, 6000),
    List(7000, 8000, 9000),
    List(10000)
  )

  test("part 1 example") {
    val actual = Day01.part1(example)
    assertEquals(actual, 24000)
  }

  test("part 1 solution") {
    val actual = Day01.runPart1()
    assertEquals(actual, 67658)
  }

  test("part 2 example") {
    val actual = Day01.part2(example)
    assertEquals(actual, 45000)
  }

  test("part 2 solution") {
    val actual = Day01.runPart2()
    assertEquals(actual, 200158)
  }
}
