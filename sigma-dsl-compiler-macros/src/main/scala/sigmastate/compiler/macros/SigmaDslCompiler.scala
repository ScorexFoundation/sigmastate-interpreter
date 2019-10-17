package sigmastate.compiler.macros

import org.ergoplatform.dsl.ContractSyntax.Proposition
import org.ergoplatform.dsl.PropositionSpec
import sigmastate.compiler.macros.impl.SigmaDslCompilerImp
import special.sigma.{Context, SigmaProp}

import scala.language.experimental.macros

object SigmaDslCompiler {

  def compile(contract: sigmastate.verification.SigmaDsl.api.sigma.Context => Boolean): (Context => Boolean, SigmaProp) = macro SigmaDslCompilerImp.compile

}
