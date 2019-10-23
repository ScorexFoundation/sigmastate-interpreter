package sigmastate.compiler.macros

import sigmastate.Values.SigmaPropValue
import sigmastate.compiler.macros.impl.SigmaDslCompilerImp
import special.sigma.{Context, SigmaProp}
import sigmastate.verification.SigmaDsl.api.sigma.{Context => VerifiedContext, SigmaProp => VerifiedSigmaProp}

import scala.language.experimental.macros

case class CompilationResult(ergoScala: Context => SigmaProp, root: SigmaPropValue)

object SigmaDslCompiler {

  def compile(verifiedContract: VerifiedContext => VerifiedSigmaProp): CompilationResult = macro SigmaDslCompilerImp.compile

}
