package sigmastate

import scorex.core.serialization.Serializer
import scorex.core.transaction.state.{Secret, SecretCompanion}
import scorex.crypto.encode.Base58

class DLogSecret extends Secret {
  override type S = DLogSecret
  override type PK = DLogProposition

  override def companion: SecretCompanion[DLogSecret] = ???

  override def publicImage: DLogProposition = ???

  override lazy val bytes: Array[Byte] = ???

  override def serializer: Serializer[M] = ???
}

case class DLogProposition(public: Array[Byte]) extends SigmaProofOfKnowledgeProposition[DLogSecret] {
  override lazy val bytes: Array[Byte] = ???

  override def toString = s"DLogKnowledge(${Base58.encode(public)})"
}