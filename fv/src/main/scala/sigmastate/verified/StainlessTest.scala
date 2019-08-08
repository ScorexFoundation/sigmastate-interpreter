package sigmastate.verified


import stainless.lang._
import stainless.collection._
import stainless.annotation._

object StainlessTest {

  case class SourceContext(line: BigInt, column: BigInt, sourceLine: String)

  def fromParserIndex(index: BigInt, lines: List[String]): SourceContext = {
    // TODO: split/simplify
    val totalLength = lines.foldLeft(BigInt(0)) {(sum, s) => sum + s.bigLength}
    if (lines.isEmpty || index < 0 || index > totalLength || lines.head.bigLength == 0)
      SourceContext(0, 0, "")
    else
      lines.tail
        .scanLeft((BigInt(0), lines.head.bigLength())) { case ((_, end), line) =>
          (end + 1, end + 1 + line.bigLength)
        }
        .zip(lines)
        .find { case ((start, end), _) => index >= start && index <= end }
        .map {
          case ((start, _), line) =>
            SourceContext(lines.indexOf(line)+ 1, index - start + 1, line)
        }.get
  }
}
