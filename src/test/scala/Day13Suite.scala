class Day13Suite extends munit.FunSuite {
  private val example = """
     |[1,1,3,1,1]
     |[1,1,5,1,1]
     |
     |[[1],[2,3,4]]
     |[[1],4]
     |
     |[9]
     |[[8,7,6]]
     |
     |[[4,4],4,4]
     |[[4,4],4,4,4]
     |
     |[7,7,7,7]
     |[7,7,7]
     |
     |[]
     |[3]
     |
     |[[[]]]
     |[[]]
     |
     |[1,[2,[3,[4,[5,6,7]]]],8,9]
     |[1,[2,[3,[4,[5,6,0]]]],8,9]
     """.stripMargin.trim

  test("part 1 example") {
    val actual = Day13.part1(example)
    assertEquals(actual, 13)
  }

  test("part 1 solution") {
    val actual = Day13.runPart1()
    assertEquals(actual, 5185)
  }

  test("part 2 example") {
    val actual = Day13.part2(example)
    assertEquals(actual, 140)
  }

  test("part 2 solution") {
    val actual = Day13.runPart2()
    assertEquals(actual, 23751)
  }
}
