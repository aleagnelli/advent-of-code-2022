class Day03Suite extends munit.FunSuite {
  private val example = """
      |vJrwpWtwJgWrhcsFMMfFFhFp
      |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
      |PmmdzqPrVvPwwTWBwg
      |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
      |ttgJtRGJQctTZtZT
      |CrZsJsPPZsGzwwsLwLmpwMDw
      """.stripMargin.trim

  test("part 1 example") {
    val actual = Day03.part1(example)
    assertEquals(actual, 157)
  }

  test("part 1 solution") {
    val actual = Day03.runPart1()
    assertEquals(actual, 8088)
  }

  test("part 2 example") {
    val actual = Day03.part2(example)
    assertEquals(actual, 70)
  }

  test("part 2 solution") {
    val actual = Day03.runPart2()
    assertEquals(actual, 2522)
  }
}
