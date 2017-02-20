package sigmastate

import scorex.core.serialization.Serializer
import scorex.core.transaction.state.{Secret, SecretCompanion}
import scorex.crypto.encode.Base58
import DlogProposition._
import scapi.sigma.rework.Challenge
import scapi.sigma.rework.DLogProtocol._

class DLogSecret extends Secret {
  override type S = DLogSecret
  override type PK = DLogProposition

  override def companion: SecretCompanion[DLogSecret] = ???

  override def publicImage: DLogProposition = ???

  override lazy val bytes: KeyPair = ???

  override def serializer: Serializer[M] = ???
}


case class DLogProposition(public: PublicKey) extends SigmaProofOfKnowledgeProposition[DLogSecret] {
  override lazy val bytes: PublicKey = public

  override def toString = s"DLogKnowledge(${Base58.encode(public)})"
}

/**
  * TODO: make implementation corresponding to RFC-8032 standard for EdDSA signatures
  * https://tools.ietf.org/html/rfc8032#page-9
  * @param signature
  */
case class SchnorrSignature(signature: Array[Byte]) extends Proof[DLogProposition] {
  override def verify(proposition: DLogProposition, message: Proof.Challenge): Boolean = {
    val x: DlogCommonInput = null
    val a: FirstDLogProverMessage = null
    val z: SecondDLogProverMessage = null
    val sigmaTranscript = DLogTranscript(x, a, Challenge(message), z)
    sigmaTranscript.accepted
  }
}


object DlogProposition {
  type PublicKey = Array[Byte]
  type PrivateKey = Array[Byte]
  type KeyPair = Array[Byte]
}