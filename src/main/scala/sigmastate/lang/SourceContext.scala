package sigmastate.lang

import scala.io.Source

case class SourceContext(line: Int, column: Int, sourceLine: String) {

}

object SourceContext {

  def fromParserIndex(index: Int, input: String): SourceContext = {
    val lines = Source.fromString(input).getLines.toSeq
    lines.tail
      .scanLeft((0, lines.head.length)) { case ((_, end), line) => (end + 1, end + line.length) }
      .zip(lines)
      .zipWithIndex
      .find { case (((start, end), _), _) =>  index >= start && index <= end }
      .map {
        case (((start, _), line), lineIndex) =>
          SourceContext(lineIndex + 1, index - start + 1, line)
      }.get
  }

}
