package sigmastate.lang

import fastparse.Parsed
import org.ergoplatform.sdk.ContractTemplate
import sigmastate.Values.SValue
import sigmastate.lang.syntax.ParserException
import org.ergoplatform.sdk.NetworkType
import sigmastate.eval.CompiletimeIRContext
import org.ergoplatform.sdk.Parameter

/** Compiler which compiles Ergo contract templates into a [[ContractTemplate]]. */
class SigmaTemplateCompiler {
  def compile(source: String): ContractTemplate = {
    ContractTemplateParser.parse(source) match {
      case Parsed.Success(template, index) =>
        val body = source.slice(index, source.length)
        val compiler = new SigmaCompiler(NetworkType.Mainnet.networkPrefix.toByte)
        implicit val ir = new CompiletimeIRContext
        val result = compiler.compile(Map.empty, body)
        assemble(template, result.buildTree)
        // SigmaParser(source.slice(index, source.length)) match {
        //   case Parsed.Success(value, _) => assemble(template, value)
        //   case f: Parsed.Failure =>
        //     throw new ParserException(s"Contract body error: $f", Some(SourceContext.fromParserFailure(f)))
        // }
      case f: Parsed.Failure =>
        throw new ParserException(s"Contract template syntax error: $f", Some(SourceContext.fromParserFailure(f)))
    }
  }

  private def assemble(parsed: ParsedContractTemplate, expr: SValue): ContractTemplate = {
    val (constTypes, constValueOpts) = parsed.signature.params.map(param => (param.tpe, param.defaultValue)).unzip
    val allConstValuesAreNone = constValueOpts.forall(_.isEmpty)
    val constValues =
      if (allConstValuesAreNone) None
      else Some(constValueOpts.toIndexedSeq)
    val contractParams = parsed.signature.params.zipWithIndex.map { case(param, idx) =>
      val desc: String = parsed.docs.params.find(docParam => docParam.name == param.name).map(_.description).getOrElse("")
      Parameter(param.name, desc, idx)
    }

    ContractTemplate(
      name = parsed.signature.name,
      description = parsed.docs.description,
      constTypes = constTypes.toIndexedSeq,
      constValues,
      contractParams.toIndexedSeq,
      expr.toSigmaProp
    )
  }
}
