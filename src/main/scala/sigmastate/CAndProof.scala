package sigmastate

import scorex.core.serialization.Serializer

/*
case class CAndProof(proofs: Proof[_ <: SigmaProposition]*) extends Proof[CAnd] {
  override val propCode: Byte = CAnd.Code

  override type M = this.type

  override def serializer: Serializer[M] = ???

  override def verify(proposition: CAnd, challenge: ProofOfKnowledge.Challenge): Boolean =
    proofs.zip(proposition.props).forall{ case (proof, prop) =>
      proof.verify()
    }
}*/
