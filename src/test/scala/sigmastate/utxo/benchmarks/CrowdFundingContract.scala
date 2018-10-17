package sigmastate.utxo.benchmarks

import org.ergoplatform.{ErgoLikeContext, ErgoLikeInterpreter}
import sigmastate.helpers.ErgoLikeProvingInterpreter
import sigmastate.interpreter.Interpreter
import sigmastate.utxo.SigmaContract

import scala.util.Try

abstract class CrowdFundingContract(
                                     val timeout: Long,
                                     val minToRaise: Long,
                                     val backerProver: ErgoLikeProvingInterpreter,
                                     val projectProver: ErgoLikeProvingInterpreter
) extends SigmaContract {
  //a blockchain node verifying a block containing a spending transaction
  val verifier = new ErgoLikeInterpreter()(backerProver.IR)
  val backerPubKey = backerProver.dlogSecrets.head.publicImage
  val projectPubKey = projectProver.dlogSecrets.head.publicImage

  def prove(ctx: ErgoLikeContext, fakeMessage: Array[Byte]): Array[Byte]

  def verify(proof: Array[Byte],
             ctx: ErgoLikeContext,
             fakeMessage: Array[Byte]): Try[Interpreter.VerificationResult]
}
