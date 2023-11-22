package sigmastate.lang

import fastparse.Parsed
import org.ergoplatform.sdk.ContractTemplate
import sigmastate.Values.SValue
import sigmastate.lang.syntax.ParserException

/** Compiler which compiles Ergo contract templates into a [[ContractTemplate]]. */
class SigmaTemplateCompiler {
  def compile(source: String): ContractTemplate = {
    ContractTemplateParser.parse(source) match {
      case Parsed.Success(value, index) =>
        val expr = SigmaParser(source.slice(index, source.length))
        assemble(value, expr)
      case f: Parsed.Failure =>
        throw new ParserException(s"Syntax error: $f", Some(SourceContext.fromParserFailure(f)))
    }
  }

  private def assemble(parsed: ParsedContractTemplate, expr: SValue): ContractTemplate = {
    val (constTypes, constValueOpts) = parsed.signature.params.map(param => (param.tpe, param.defaultValue)).unzip
    val allConstValuesAreNone = constValueOpts.forall(_.isEmpty)
    // ContractTemplate defines this as SType, is that right? Should it be Constant instead?
    val constValues =
      if (allConstValuesAreNone) None
      else Some(constValueOpts.toIndexedSeq)

    val params = IndexedSeq()

    ContractTemplate(
      name = parsed.signature.name,
      description = parsed.docs.description,
      constTypes = constTypes.toIndexedSeq,
      None,
      params,
      expr.toSigmaProp
    )
  }
}