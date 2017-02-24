package sigmastate

import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition}
import scorex.core.transaction.state.Secret

trait SigmaStateProposition extends Proposition {
  override def serializer: Serializer[M] = ???
}

trait StateProposition extends SigmaStateProposition

trait SigmaProposition extends SigmaStateProposition{
  type SP >: this.type <: SigmaProposition
}

trait CompoundSigmaProposition extends SigmaProposition

case class CAnd(statements: SigmaProposition*) extends CompoundSigmaProposition

case class COr(statements: SigmaProposition*) extends CompoundSigmaProposition

trait SigmaProofOfKnowledgeProposition[S <: Secret] extends SigmaProposition with ProofOfKnowledgeProposition[S]

trait Proof[CP <: SigmaProposition] {
  def verify(proposition: CP, challenge: Proof.Challenge): Boolean
}

object Proof {
  type Challenge = Array[Byte]
}

case class Or(statement1: SigmaStateProposition, statement2: SigmaStateProposition) extends SigmaStateProposition {
  override lazy val bytes: Array[Byte] = ???
}

case class And(statement1: SigmaStateProposition, statement2: SigmaStateProposition) extends SigmaStateProposition {
  override lazy val bytes: Array[Byte] = ???
}
