package sigmastate

import scapi.sigma.rework._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition}


trait SigmaStateProposition extends Proposition {
  override def serializer: Serializer[M] = ???
}

case class Or(statement1: SigmaStateProposition, statement2: SigmaStateProposition) extends SigmaStateProposition {
  override lazy val bytes: Array[Byte] = ???
}

case class And(statement1: SigmaStateProposition, statement2: SigmaStateProposition) extends SigmaStateProposition {
  override lazy val bytes: Array[Byte] = ???
}

trait StateProposition extends SigmaStateProposition

trait SigmaProposition extends SigmaStateProposition {
  val code: SigmaProposition.PropositionCode
}

object SigmaProposition {
  type PropositionCode = Byte
}

trait SigmaProofOfKnowledgeProposition[SP <: SigmaProtocol[SP], S <: SigmaProtocolPrivateInput[SP]]
  extends SigmaProposition with ProofOfKnowledgeProposition[S]  with SigmaProtocolCommonInput[SP]