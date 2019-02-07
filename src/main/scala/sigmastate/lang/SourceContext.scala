package sigmastate.lang

case class SourceContext(line: Int, column: Int, sourceLine: String) {

}

object SourceContext {
  def apply(parserIndex: Int, input: String): SourceContext = SourceContext(parserIndex, 0, input)
}
