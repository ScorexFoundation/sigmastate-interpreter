package sigmastate.compiler.macros.test

import sigmastate.Values.ErgoTree
import sigmastate.compiler.macros.impl.ErgoContractCompiler
import sigmastate.verification.SigmaDsl.api.sigma.{Context => VerifiedContext, SigmaProp => VerifiedSigmaProp}
import special.sigma.{Context, SigmaProp}

import scala.language.experimental.macros

case class ErgoContract(scalaFunc: Context => SigmaProp, ergoTree: ErgoTree)

object ErgoContract {

  def apply(verifiedContract: VerifiedContext => VerifiedSigmaProp): ErgoContract = {
    val (scalaFunc, sigmaProp) = ErgoContractCompiler.compile(verifiedContract)
   val ergoTree = ErgoTree.fromProposition(sigmaProp)
    ErgoContract(scalaFunc, ergoTree)
  }

}
