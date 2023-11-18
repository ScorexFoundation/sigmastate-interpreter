package sigmastate.lang

import org.ergoplatform.sdk.ContractTemplate

/** Compiler which compiles Ergo contract templates into a [[ContractTemplate]]. */
class SigmaTemplateCompiler {
  def compile(template: String): ContractTemplate = {
    // parsedTemplate = ContractTemplateParser.parse
    // create & return ContractTemplate from parsed template
    ???
  }
}

object SigmaTemplateCompiler {
  def apply: SigmaTemplateCompiler =
    new SigmaTemplateCompiler
}