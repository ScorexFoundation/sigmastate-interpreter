package sigmastate.utxo.benchmarks

import org.ergoplatform.{ErgoContext, ErgoInterpreter}
import sigmastate.helpers.ErgoProvingInterpreter
import sigmastate.interpreter.Interpreter
import sigmastate.utxo.SigmaContract

import scala.util.Try

abstract class CrowdFundingContract(
    val timeout: Long,
    val minToRaise: Long,
    val backerProver: ErgoProvingInterpreter,
    val projectProver: ErgoProvingInterpreter
) extends SigmaContract {
  //a blockchain node verifying a block containing a spending transaction
  val verifier = new ErgoInterpreter
  val backerPubKey = backerProver.dlogSecrets.head.publicImage
  val projectPubKey = projectProver.dlogSecrets.head.publicImage

  def prove(ctx: ErgoContext, fakeMessage: Array[Byte]): projectProver.ProofT

  def verify(proof: projectProver.ProofT,
             ctx: ErgoContext,
             fakeMessage: Array[Byte]): Try[Interpreter.VerificationResult]
}
