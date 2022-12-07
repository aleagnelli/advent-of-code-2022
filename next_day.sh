#!/usr/bin/env bash

last_day="10#$(find src/main/scala/Day* | sort -r | head -n1 | grep -oE '([0-9]){2}')"
next_day=$((last_day + 1))
day=$(printf "%02d" "${next_day}")

cat <<EOF >"src/main/scala/Day${day}.scala"
import scala.io.Source

object Day${day} {
  private val input = Source
    .fromResource("day${day}.txt")
    .mkString

  def runPart1(): Int = part1(input)

  def part1(s: String) = ???

  // def runPart2(): Int = part2(input)

  // def part2(s: String) = ???
}
EOF

touch "src/main/resources/day${day}.txt"

cat <<EOF >"src/test/scala/Day${day}Suite.scala"
class Day${day}Suite extends munit.FunSuite {
  test("part 1 example") {
    val actual = Day${day}.part1("")
    assertEquals(actual, 0)
  }

  test("part 1 solution") {
    val actual = Day${day}.runPart1()
    assertEquals(actual, 0)
  }

  // test("part 2 example") {
  //   val actual = Day${day}.part2("")
  //   assertEquals(actual, 0)
  // }

  // test("part 2 solution") {
  //   val actual = Day${day}.runPart2()
  //   assertEquals(actual, 0)
  // }
}
EOF
