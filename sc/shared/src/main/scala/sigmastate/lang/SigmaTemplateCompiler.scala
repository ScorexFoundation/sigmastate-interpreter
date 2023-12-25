package sigmastate.lang

import fastparse.Parsed
import org.ergoplatform.sdk.ContractTemplate
import sigmastate.eval.CompiletimeIRContext
import org.ergoplatform.sdk.Parameter
import sigma.ast.SourceContext
import sigma.ast.syntax.SValue
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.parsers.ParserException

/** Compiler which compiles Ergo contract templates into a [[ContractTemplate]]. */
class SigmaTemplateCompiler(networkPrefix: Byte) {
  val sigmaCompiler = new SigmaCompiler(networkPrefix)

  /**
   * Compiles the provided contract source code into a [[ContractTemplate]].
   *
   * @param source The ErgoScript contract source code.
   * @return The contract template.
   */
  def compile(env: ScriptEnv, source: String): ContractTemplate = {
    ContractParser.parse(source) match {
      case Parsed.Success(template, _) => {
        implicit val ir = new CompiletimeIRContext
        val result = sigmaCompiler.compileParsed(env, template.body)
        assemble(template, result.buildTree)
      }
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
    val contractParams = parsed.signature.params.zipWithIndex.map { case (param, idx) =>
      val desc: String = parsed.docs.params.find(docParam => docParam.name == param.name).map(_.description).getOrElse("")
      Parameter(param.name, desc, idx)
    }

    ContractTemplate(
      name = parsed.signature.name,
      description = parsed.docs.description,
      constTypes = constTypes.toIndexedSeq,
      constValues = constValues,
      parameters = contractParams.toIndexedSeq,
      expressionTree = expr.toSigmaProp
    )
  }
}

object SigmaTemplateCompiler {
  def apply(networkPrefix: Byte): SigmaTemplateCompiler = new SigmaTemplateCompiler(networkPrefix)
}
