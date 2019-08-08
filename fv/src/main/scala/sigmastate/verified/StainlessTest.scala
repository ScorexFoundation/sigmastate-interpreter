package sigmastate.verified


import stainless.lang._
import stainless.collection._
import stainless.annotation._

object StainlessTest {

  case class SourceContext(line: BigInt, column: BigInt, sourceLine: String)

  def totalLength(lines: List[String]): BigInt = {
    lines.foldLeft(BigInt(0)) { (sum, s) => sum + s.bigLength }
  }

  def fromParserIndex(index: BigInt, lines: List[String]): SourceContext = {
    if (lines.isEmpty)
        SourceContext(0, 0, "")
    else
      lines.tail
        .scanLeft((BigInt(0), lines.head.bigLength(), lines.head)) { case ((_, end, _), line) =>
          (end + 1, end + 1 + line.bigLength, line)
        }
        .find { case (start, end, _) => index >= start && index <= end }
        .map {
          case (start, _, line) =>
            SourceContext(lines.indexOf(line) + 1, index - start + 1, line)
        }.getOrElse(SourceContext(0, 0, ""))
  }

}
