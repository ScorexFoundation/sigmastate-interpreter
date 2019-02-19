package sigmastate.lang

import fastparse.core.Parsed.Failure

import scala.io.Source

case class SourceContext(line: Int, column: Int, sourceLine: String) {

}

object SourceContext {

  def fromParserIndex(index: Int, input: String): SourceContext = {
    val lines = Source.fromString(input).getLines.toSeq
    if (lines.isEmpty)
      SourceContext(0, 0, "")
    else
      lines.tail
        .scanLeft((0, lines.head.length)) { case ((_, end), line) => (end + 1, end + 1 + line.length) }
        .zip(lines)
        .zipWithIndex
        .find { case (((start, end), _), _) =>  index >= start && index <= end }
        .map {
          case (((start, _), line), lineIndex) =>
            SourceContext(lineIndex + 1, index - start + 1, line)
        }.get
  }

  def fromParserFailure(e: Failure[_, String]): SourceContext =
    fromParserIndex(e.index , e.extra.input.slice(0, e.extra.input.length))
}
