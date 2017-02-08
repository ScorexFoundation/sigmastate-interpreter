package sigmastate

import scorex.core.serialization.Serializer
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition}
import scorex.core.transaction.state.{Secret, SecretCompanion}
import scorex.crypto.encode.Base58


trait SigmaStateProposition extends Proposition {
  override def serializer: Serializer[M] = ???
}

trait StateProposition extends SigmaStateProposition

trait SigmaProposition extends SigmaStateProposition

case class CAnd(statement1: SigmaProposition, statement2: SigmaProposition) extends SigmaProposition

case class COr(statement1: SigmaProposition, statement2: SigmaProposition) extends SigmaProposition

trait SigmaProofOfKnowledgeProposition[S <: Secret] extends SigmaProposition with ProofOfKnowledgeProposition[S]

trait Proof[CP <: SigmaProposition] {
  def verify(proposition: CP): Boolean
}


class DLogSecret extends Secret {
  override type S = DLogSecret
  override type PK = DLogPublic

  override def companion: SecretCompanion[DLogSecret] = ???

  override def publicImage: DLogPublic = ???

  override lazy val bytes: Array[Byte] = ???

  override def serializer: Serializer[M] = ???
}

case class DLogPublic(public: Array[Byte]) extends SigmaProofOfKnowledgeProposition[DLogSecret] {
  override lazy val bytes: Array[Byte] = ???

  override def toString = s"DLogKnowledge(${Base58.encode(public)})"
}

case class HeightFromProposition(from: Int) extends StateProposition {
  override lazy val bytes: Array[Byte] = ???
}

case class HeightUntilProposition(until: Int) extends StateProposition {
  override lazy val bytes: Array[Byte] = ???
}

case class HeightBetweenProposition(from: Int, until: Int) extends StateProposition {
  override lazy val bytes: Array[Byte] = ???
}

case class TransactionContainsBox(minAmountOpt: Option[Box.Amount], maxAmountOpt: Option[Box.Amount])

case class Or(statement1: SigmaStateProposition, statement2: SigmaStateProposition) extends SigmaStateProposition {
  override lazy val bytes: Array[Byte] = ???
}

case class And(statement1: SigmaStateProposition, statement2: SigmaStateProposition) extends SigmaStateProposition {
  override lazy val bytes: Array[Byte] = ???
}