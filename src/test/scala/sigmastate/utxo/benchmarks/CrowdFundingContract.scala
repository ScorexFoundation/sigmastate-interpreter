package sigmastate.utxo.benchmarks

import org.ergoplatform.ErgoLikeContext
import sigmastate.helpers.ErgoLikeTestProvingInterpreter
import sigmastate.interpreter.Interpreter
import sigmastate.utxo.{ErgoLikeTestInterpreter, SigmaContract}

import scala.util.Try

abstract class CrowdFundingContract(
                                     val timeout: Int,
                                     val minToRaise: Long,
                                     val backerProver: ErgoLikeTestProvingInterpreter,
                                     val projectProver: ErgoLikeTestProvingInterpreter
) extends SigmaContract {
  //a blockchain node verifying a block containing a spending transaction
  val verifier = new ErgoLikeTestInterpreter()(backerProver.IR)
  val backerPubKey = backerProver.dlogSecrets.head.publicImage
  val projectPubKey = projectProver.dlogSecrets.head.publicImage

  def prove(ctx: ErgoLikeContext, fakeMessage: Array[Byte]): Array[Byte]

  def verify(proof: Array[Byte],
             ctx: ErgoLikeContext,
             fakeMessage: Array[Byte]): Try[Interpreter.VerificationResult]
}
