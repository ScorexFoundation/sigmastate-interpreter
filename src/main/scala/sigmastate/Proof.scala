package sigmastate

import scapi.sigma.rework.SigmaProtocol
import scorex.core.serialization.BytesSerializable


trait Proof[P <: SigmaProposition] extends BytesSerializable {
  val proposition: P
  val challenge: ProofOfKnowledge.Challenge

  def verify(): Boolean

  val propCode: SigmaProposition.PropositionCode
}

trait ProofOfKnowledge[SP <: SigmaProtocol[SP], CI <: SigmaProofOfKnowledgeTree[SP, _]]
  extends Proof[CI]

object ProofOfKnowledge {
  type Challenge = Array[Byte]
}

