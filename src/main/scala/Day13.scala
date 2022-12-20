import scala.io.Source

object Day13 {
  private trait Packet
  private case class PList(list: List[Packet]) extends Packet
  private case class PInt(value: Int) extends Packet

  opaque type Signal = List[Packet]

  private enum Compare:
    case Ordered
    case NotOrdered
    case Undefined

  private val input = Source
    .fromResource("day13.txt")
    .mkString

  def runPart1(): Int = part1(input)

  def part1(input: String) = {
    val signal = parseSignal(input)
      .sliding(2, 2)
      .toList
      .map {
        case l :: r :: Nil => (l, r)
        case _             => throw IllegalStateException()
      }
    compare(signal).zipWithIndex
      .map { case (r, i) => (r, i + 1) }
      .filter { case (r, _) => r }
      .map(_._2)
      .sum
  }

  def runPart2(): Int = part2(input)

  def part2(input: String) = {
    val signal = parseSignal(input)

    val markers = List(parsePacket("[[2]]"), parsePacket("[[6]]")).map(_._1)
    (signal ++ markers)
      .sortWith((l, r) => isOrderedBool(l, r))
      .zipWithIndex
      .filter { case (p, _) => markers.contains(p) }
      .map { case (_, i) => i + 1 }
      .reduce(_ * _)
  }

  private def parseSignal(rawSignal: String): Signal = {
    rawSignal
      .split("\n")
      .filterNot(_.isEmpty)
      .map(x => parsePacket(x)._1)
      .toList
  }

  private def parsePacket(rawPacket: String): (Packet, String) =
    if rawPacket.charAt(0).isDigit then
      val i = rawPacket.takeWhile(_.isDigit).toInt
      (PInt(i), rawPacket.dropWhile(_.isDigit))
    else if rawPacket.charAt(0) == '[' then
      def help(remPacket: String, packetList: List[Packet]): (List[Packet], String) =
        if remPacket.charAt(0) == ']' then (packetList.reverse, remPacket.drop(1))
        else if remPacket.charAt(0) == ',' then help(remPacket.drop(1), packetList)
        else
          val (listParsed, remaining) = parsePacket(remPacket)
          help(remaining, listParsed +: packetList)

      val (list, rem) = help(rawPacket.drop(1), List.empty)
      (PList(list), rem)
    else (PList(List.empty[Packet]), rawPacket)

  private def compare(signal: List[(Packet, Packet)]): List[Boolean] =
    signal.map { case (left, right) => isOrderedBool(left, right) }

  private def isOrderedBool(left: Packet, right: Packet): Boolean =
    isOrdered(left, right) match
      case Compare.Ordered    => true
      case Compare.NotOrdered => false
      case Compare.Undefined  => throw IllegalStateException()

  private def isOrdered(left: Packet, right: Packet): Compare =
    (left, right) match
      case (PInt(v1), PInt(v2)) if v1 == v2 => Compare.Undefined
      case (PInt(v1), PInt(v2)) if v1 < v2  => Compare.Ordered
      case (PInt(v1), PInt(v2)) if v1 > v2  => Compare.NotOrdered
      case (l @ PList(_), r @ PInt(_))      => isOrdered(l, PList(List(r)))
      case (l @ PInt(_), r @ PList(_))      => isOrdered(PList(List(l)), r)
      case (PList(l), PList(r))             => isListOrdered(l, r)

  private def isListOrdered(left: List[Packet], right: List[Packet]): Compare = {
    (left, right) match
      case (Nil, Nil)      => Compare.Undefined
      case ((_ :: _), Nil) => Compare.NotOrdered
      case (Nil, (_ :: _)) => Compare.Ordered
      case (v1 :: t1, v2 :: t2) =>
        isOrdered(v1, v2) match
          case Compare.Undefined => isListOrdered(t1, t2)
          case c                 => c
  }
}
