class Day05Suite extends munit.FunSuite {
  private val example = """
      |    [D]    
      |[N] [C]    
      |[Z] [M] [P]
      | 1   2   3 
      |
      |move 1 from 2 to 1
      |move 3 from 1 to 3
      |move 2 from 2 to 1
      |move 1 from 1 to 2""".stripMargin

  test("part 1 example") {
    val actual = Day05.part1(example)
    assertEquals(actual, "CMZ")
  }

  test("part 1 solution") {
    val actual = Day05.runPart1()
    assertEquals(actual, "QGTHFZBHV")
  }

  test("part 2 example") {
    val actual = Day05.part2(example)
    assertEquals(actual, "MCD")
  }

  test("part 2 solution") {
    val actual = Day05.runPart2()
    assertEquals(actual, "MGDMPSZTM")
  }
}
