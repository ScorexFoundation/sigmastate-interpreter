package sigmastate.utxo

import scapi.sigma.rework.DLogProtocol.{DLogCommonInput, DLogSigmaProtocol}
import scorex.core.serialization.Serializer
import sigmastate.ProofOfKnowledge.Challenge
import sigmastate.SigmaProposition.PropositionCode
import sigmastate._


case class TestingReducerInput(override val height: Int) extends BlockchainState


object TestingInterpreter extends Interpreter {
  override type SProp = StateProposition
  override type Context = TestingReducerInput


  override type CProp = DLogCommonInput
  override type CProof = FakeSchnorrSignature.type

  override val maxDepth = 50

  override def statefulReductions[SP <: StateProposition](proposition: SP, environment: TestingReducerInput): BooleanConstantProposition =
    proposition match {
      case HeightFromProposition(from) =>
        if (environment.height >= from) TrueProposition else FalseProposition
      case HeightBetweenProposition(from, until) =>
        if (environment.height >= from && environment.height < until) TrueProposition else FalseProposition
      case HeightUntilProposition(until) =>
        if (environment.height < until) TrueProposition else FalseProposition
    }
}


object FakeSchnorrSignature extends ProofOfKnowledge[DLogSigmaProtocol, DLogCommonInput] {
  override val propCode: PropositionCode = DLogCommonInput.Code

  override def verify(proposition: DLogCommonInput, challenge: Challenge): Boolean =
    proposition.bytes.sameElements(challenge)

  override type M = this.type

  override def serializer: Serializer[FakeSchnorrSignature.type] = ???
}