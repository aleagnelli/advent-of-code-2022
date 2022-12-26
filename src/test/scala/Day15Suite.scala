class Day15Suite extends munit.FunSuite {
  private val example = """
      |Sensor at x=2, y=18: closest beacon is at x=-2, y=15
      |Sensor at x=9, y=16: closest beacon is at x=10, y=16
      |Sensor at x=13, y=2: closest beacon is at x=15, y=3
      |Sensor at x=12, y=14: closest beacon is at x=10, y=16
      |Sensor at x=10, y=20: closest beacon is at x=10, y=16
      |Sensor at x=14, y=17: closest beacon is at x=10, y=16
      |Sensor at x=8, y=7: closest beacon is at x=2, y=10
      |Sensor at x=2, y=0: closest beacon is at x=2, y=10
      |Sensor at x=0, y=11: closest beacon is at x=2, y=10
      |Sensor at x=20, y=14: closest beacon is at x=25, y=17
      |Sensor at x=17, y=20: closest beacon is at x=21, y=22
      |Sensor at x=16, y=7: closest beacon is at x=15, y=3
      |Sensor at x=14, y=3: closest beacon is at x=15, y=3
      |Sensor at x=20, y=1: closest beacon is at x=15, y=3
      """.stripMargin.trim

  test("part 1 example") {
    val actual = Day15.part1(example, 10)
    assertEquals(actual, 26)
  }

  test("part 1 solution") {
    val actual = Day15.runPart1()
    assertEquals(actual, 4717631)
  }

  test("part 2 example") {
    val actual = Day15.part2(example, 20)
    assertEquals(actual, 56000011L)
  }

  test("part 2 solution") {
    val actual = Day15.runPart2()
    assertEquals(actual, 13197439355220L)
  }
}
