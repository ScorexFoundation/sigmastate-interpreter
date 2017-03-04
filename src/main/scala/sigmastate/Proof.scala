package sigmastate

import scapi.sigma.rework.SigmaProtocol
import scorex.core.serialization.BytesSerializable


trait Proof[P <: SigmaProposition] extends BytesSerializable {
  def verify(proposition: P, challenge: ProofOfKnowledge.Challenge): Boolean

  val propCode: SigmaProposition.PropositionCode
}

trait ProofOfKnowledge[SP <: SigmaProtocol[SP], CI <: SigmaProofOfKnowledgeProposition[SP, _]]
  extends Proof[CI]

object ProofOfKnowledge {
  type Challenge = Array[Byte]
}

