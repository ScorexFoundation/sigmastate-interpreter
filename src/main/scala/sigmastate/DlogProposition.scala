package sigmastate

import scorex.core.serialization.Serializer
import scorex.core.transaction.state.{Secret, SecretCompanion}
import scorex.crypto.encode.Base58
import DlogProposition._
import sigmastate.Proof.Challenge

class DLogSecret extends Secret {
  override type S = DLogSecret
  override type PK = DLogProposition

  override def companion: SecretCompanion[DLogSecret] = ???

  override def publicImage: DLogProposition = ???

  override lazy val bytes: KeyPair = ???

  override def serializer: Serializer[M] = ???
}


case class DLogProposition(public: PublicKey) extends SigmaProofOfKnowledgeProposition[DLogSecret] {
  override lazy val bytes: PublicKey = ???

  override def toString = s"DLogKnowledge(${Base58.encode(public)})"
}

case class SchnorrSignature(signature: Array[Byte]) extends Proof[DLogProposition] {
  override def verify(proposition: DLogProposition, challenge: Challenge): Boolean = ???
}


object DlogProposition {
  type PublicKey = Array[Byte]
  type PrivateKey = Array[Byte]
  type KeyPair = Array[Byte]
}