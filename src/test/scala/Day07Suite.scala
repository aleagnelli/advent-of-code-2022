class Day07Suite extends munit.FunSuite {
  private val example = """
      |$ cd /
      |$ ls
      |dir a
      |14848514 b.txt
      |8504156 c.dat
      |dir d
      |$ cd a
      |$ ls
      |dir e
      |29116 f
      |2557 g
      |62596 h.lst
      |$ cd e
      |$ ls
      |584 i
      |$ cd ..
      |$ cd ..
      |$ cd d
      |$ ls
      |4060174 j
      |8033020 d.log
      |5626152 d.ext
      |7214296 k
      """.stripMargin.trim

  test("part 1 example") {
    val actual = Day07.part1(example)
    assertEquals(actual, 95437)
  }

  test("part 1 solution") {
    val actual = Day07.runPart1()
    assertEquals(actual, 1770595)
  }

  test("part 2 example") {
    val actual = Day07.part2(example)
    assertEquals(actual, 24933642)
  }

  test("part 2 solution") {
    val actual = Day07.runPart2()
    assertEquals(actual, 2195372)
  }
}
