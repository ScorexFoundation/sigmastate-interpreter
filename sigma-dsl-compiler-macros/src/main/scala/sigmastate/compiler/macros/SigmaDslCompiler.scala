package sigmastate.compiler.macros

import org.ergoplatform.dsl.PropositionSpec
import sigmastate.compiler.macros.impl.SigmaDslCompilerImp
import sigmastate.verification.SigmaDsl.api.sigma.Context

import scala.language.experimental.macros

object SigmaDslCompiler {

  def compile(contract: Context => Boolean): PropositionSpec = macro SigmaDslCompilerImp.compile

}
