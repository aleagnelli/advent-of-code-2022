class Day06Suite extends munit.FunSuite {
  test("part 1 example") {
    assertEquals(Day06.part1("mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 7)
    assertEquals(Day06.part1("bvwbjplbgvbhsrlpgdmjqwftvncz"), 5)
    assertEquals(Day06.part1("nppdvjthqldpwncqszvftbrmjlhg"), 6)
    assertEquals(Day06.part1("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), 10)
    assertEquals(Day06.part1("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), 11)
  }

  test("part 1 solution") {
    val actual = Day06.runPart1()
    assertEquals(actual, 1723)
  }

  test("part 2 example") {
    assertEquals(Day06.part2("mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 19)
    assertEquals(Day06.part2("bvwbjplbgvbhsrlpgdmjqwftvncz"), 23)
    assertEquals(Day06.part2("nppdvjthqldpwncqszvftbrmjlhg"), 23)
    assertEquals(Day06.part2("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), 29)
    assertEquals(Day06.part2("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), 26)
  }

  test("part 2 solution") {
    val actual = Day06.runPart2()
    assertEquals(actual, 3708)
  }
}
