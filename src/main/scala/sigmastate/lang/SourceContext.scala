package sigmastate.lang

import sigmastate.verified.StainlessTest
import fastparse.core.Parsed.Failure

import scala.io.Source

case class SourceContext(line: Int, column: Int, sourceLine: String) {

}

object SourceContext {

  def fromParserIndex(index: Int, input: String): SourceContext = {
    val scbi = StainlessTest.fromParserIndex(BigInt(index), Source.fromString(input).getLines.toArray)
    SourceContext(scbi.line.toInt, scbi.column.toInt, scbi.sourceLine)
  }

  def fromParserFailure(e: Failure[_, String]): SourceContext =
    fromParserIndex(e.index , e.extra.input.slice(0, e.extra.input.length))
}
