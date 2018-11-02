package sigmastate.utxo.benchmarks

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import org.ergoplatform.ErgoLikeContext
import sigmastate.SBoolean
import sigmastate.Values.Value
import sigmastate.helpers.ErgoLikeTestProvingInterpreter
import sigmastate.interpreter.Interpreter
import sigmastate.interpreter.Interpreter._
import sigmastate.lang.Terms._

import scala.util.Try

class CrowdFundingScriptContract(
                                  timeout: Long,
                                  minToRaise: Long,
                                  override val backerProver: ErgoLikeTestProvingInterpreter,
                                  override val projectProver: ErgoLikeTestProvingInterpreter
) extends CrowdFundingContract(timeout, minToRaise, backerProver, projectProver) {

  val compiledProposition: Value[SBoolean.type] = {
    val env = Map(
      "timeout" -> timeout,
      "minToRaise" -> minToRaise,
      "backerPubKey" -> backerPubKey,
      "projectPubKey" -> projectPubKey
    )
    val compiledScript = compiler.compile(env,
      """{
       | val c1 = HEIGHT >= timeout && backerPubKey
       | val c2 = allOf(Array(
       |   HEIGHT < timeout,
       |   projectPubKey,
       |   OUTPUTS.exists({ (out: Box) =>
       |     out.value >= minToRaise && out.propositionBytes == projectPubKey.propBytes
       |   })
       | ))
       | c1 || c2
       | }
      """.stripMargin,
      TestnetNetworkPrefix).asBoolValue
    compiledScript
  }

  def prove(ctx: ErgoLikeContext, fakeMessage: Array[Byte]): Array[Byte] = {
    val proofP = projectProver.prove(compiledProposition, ctx, fakeMessage).get.proof
    proofP
  }

  def verify(proof: Array[Byte],
             ctx: ErgoLikeContext,
             fakeMessage: Array[Byte]): Try[Interpreter.VerificationResult] = {
    val res = verifier.verify(emptyEnv, compiledProposition, ctx, proof, fakeMessage)
    res
  }
}
