import scala.io.Source

object Day08 {
  private val input = Source
    .fromResource("day08.txt")
    .mkString

  private enum Visibility {
    case Visible extends Visibility
    case NotVisible extends Visibility
  }

  private case class Tree(
      height: Int,
      visibility: Visibility = Visibility.NotVisible,
      scenicScore: Int = 1
  ) {
    def addViewingDistance(d: Int) = this.copy(scenicScore = scenicScore * d)
  }

  def runPart1(): Int = part1(input)

  def part1(input: String) = {
    val trees = parseGrid(input)
    val markFromL = trees.map(markVisible)
    val markFromLR = markFromL.map(_.reverse).map(markVisible)
    val treeTransposed = markFromLR.transpose
    val markFromLRU = treeTransposed.map(markVisible)
    val markFromLRUD = markFromLRU.map(_.reverse).map(markVisible)
    markFromLRUD.map(_.count(_.visibility == Visibility.Visible)).sum
  }

  private def markVisible(trees: List[Tree]): List[Tree] = {
    trees.reverse.tails
      .filterNot(_.isEmpty)
      .map(line => {
        if (line.length == 1) {
          line.head.copy(visibility = Visibility.Visible)
        } else {
          val tree = line.head
          val before = line.tail
          if (tree.height > before.maxBy(_.height).height) {
            tree.copy(visibility = Visibility.Visible)
          } else {
            tree
          }
        }
      })
      .toList
      .reverse
  }

  def runPart2(): Int = part2(input)

  def part2(input: String) = {
    val trees = parseGrid(input)
    val viewFromL = trees.map(findViewingDistance)
    val viewFromLR = viewFromL.map(_.reverse).map(findViewingDistance).map(_.reverse)
    val treeTransposed = viewFromLR.transpose
    val viewFromLRU = treeTransposed.map(findViewingDistance)
    val viewFromLRUD = viewFromLRU.map(_.reverse).map(findViewingDistance).map(_.reverse).transpose
    viewFromLRUD.map(line => line.maxBy(_.scenicScore).scenicScore).max
  }

  private def parseGrid(input: String) =
    input
      .split("\n")
      .map(_.toCharArray.map(h => Tree(h.toString.toInt)).toList)
      .toList

  private def findViewingDistance(trees: List[Tree]) = {
    trees.tails
      .filterNot(_.isEmpty)
      .map(line => {
        val tree = line.head
        val after = line.tail
        val (viewingDistance, _) =
          after.foldLeft((0, false)) {
            case ((counter, true), seeing) =>
              (counter, true)
            case ((counter, false), seeing) if seeing.height < tree.height =>
              (counter + 1, false)
            case ((counter, false), seeing) if seeing.height >= tree.height =>
              (counter + 1, true)
            case ((counter, false), _) =>
              (counter, true) // should never happen, just satisfy the exhaustiveness
          }
        tree.addViewingDistance(viewingDistance)
      })
      .toList
  }
}
