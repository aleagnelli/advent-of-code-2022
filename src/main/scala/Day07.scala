import scala.io.Source
import scala.collection.immutable.ListMap

object Day07 {
  private trait AOCFileSystem
  private case class File(size: Int) extends AOCFileSystem
  private case class Directory(ls: List[String]) extends AOCFileSystem

  private case class Path(val path: List[String] = List.empty) {
    def cd(f: String) = Path(f +: path)
    def parent = Path(path.drop(1))
    def fqn = path.reverse.mkString("/").replace("//", "/")
  }

  private val input = Source
    .fromResource("day07.txt")
    .mkString

  def runPart1(): Int = part1(input)

  def part1(terminalOutput: String) = {
    val fs = parseFileSystem(terminalOutput)

    fs.filter(_._2.isInstanceOf[Directory])
      .keys
      .map(dir => size(fs, dir))
      .filter(size => size <= 100000)
      .sum
  }

  def runPart2(): Int = part2(input)

  def part2(terminalOutput: String) = {
    val fs = parseFileSystem(terminalOutput)
    val totalSpaceAvailable = 70000000
    val neededUnusedSpace = 30000000

    val totalUsed = size(fs)
    val minSpaceNeeded = neededUnusedSpace - (totalSpaceAvailable - totalUsed)

    fs.filter(_._2.isInstanceOf[Directory])
      .keys
      .map(dir => size(fs, dir))
      .toList
      .sorted
      .find(size => size >= minSpaceNeeded)
      .get
  }

  private def parseFileSystem(terminalOutput: String): Map[String, AOCFileSystem] = {
    val (fs, _) = terminalOutput
      .split("\\$")
      .map(_.trim)
      .drop(1)
      .foldLeft((ListMap[String, AOCFileSystem](), Path())) { case ((fs, currentPath), command) =>
        command match {
          case "cd .." =>
            (fs, currentPath.parent)
          case s"cd $name" =>
            (fs, currentPath.cd(name))
          case s"ls$lsOutput" => {
            val files = lsOutput.trim
              .split("\n")
              .map {
                case s"dir $name"   => (currentPath.cd(name).fqn, Directory(List()))
                case s"$size $name" => (currentPath.cd(name).fqn, File(size.toInt))
              }
              .toMap

            val newFS = fs ++ files + (currentPath.fqn -> Directory(files.keys.toList))
            (newFS, currentPath)
          }
        }
      }
    fs
  }

  private def size(fs: Map[String, AOCFileSystem], dir: String = "/"): Int = {
    fs(dir)
      .asInstanceOf[Directory]
      .ls
      .map(name =>
        fs(name) match {
          case f @ File(size) => size
          case Directory(_)   => size(fs, name)
        }
      )
      .sum
  }
}
