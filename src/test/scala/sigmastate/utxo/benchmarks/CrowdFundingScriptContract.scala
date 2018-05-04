package sigmastate.utxo.benchmarks

import sigmastate.SBoolean
import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.helpers.ErgoProvingInterpreter
import sigmastate.interpreter.Interpreter
import sigmastate.utxo.ErgoContext

import scala.util.Try

class CrowdFundingScriptContract(
    timeout: Long,
    minToRaise: Long,
    override val backerProver: ErgoProvingInterpreter,
    override val projectProver: ErgoProvingInterpreter
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
       | let c1 = HEIGHT >= timeout && backerPubKey
       | let c2 = allOf(Array(
       |   HEIGHT < timeout,
       |   projectPubKey,
       |   OUTPUTS.exists(fun (out: Box) = {
       |     out.value >= minToRaise && out.propositionBytes == projectPubKey.propBytes
       |   })
       | ))
       | c1 || c2
       | }
      """.stripMargin).asBoolValue
    compiledScript
  }

  def prove(ctx: ErgoContext, fakeMessage: Array[Byte]): this.projectProver.ProofT = {
    val proofP = projectProver.prove(compiledProposition, ctx, fakeMessage).get.proof
    proofP
  }

  def verify(proof: projectProver.ProofT,
             ctx: ErgoContext,
             fakeMessage: Array[Byte]): Try[Interpreter.VerificationResult] = {
    val res = verifier.verify(compiledProposition, ctx, proof, fakeMessage)
    res
  }
}
