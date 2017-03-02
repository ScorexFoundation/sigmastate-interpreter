package sigmastate

import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition}
import scorex.core.transaction.state.Secret

trait SigmaStateProposition extends Proposition {
  override def serializer: Serializer[M] = ???
}

trait StateProposition extends SigmaStateProposition

trait SigmaProposition extends SigmaStateProposition {
  val code: SigmaProposition.PropositionCode
  type SP >: this.type <: SigmaProposition
}

object SigmaProposition {
  type PropositionCode = Byte
}

trait CompoundSigmaProposition extends SigmaProposition

case class CAnd(statements: SigmaProposition*) extends CompoundSigmaProposition {
  override val code = CAnd.Code
}

object CAnd {
  val Code = 100: Byte
}

case class COr(statements: SigmaProposition*) extends CompoundSigmaProposition {
  override val code = COr.Code
}

object COr {
  val Code = 101: Byte
}

trait SigmaProofOfKnowledgeProposition[S <: Secret] extends SigmaProposition with ProofOfKnowledgeProposition[S]

trait Proof[CP <: SigmaProposition] extends BytesSerializable {
  val propCode: SigmaProposition.PropositionCode

  def verify(proposition: CP, challenge: Proof.Challenge): Boolean
}

object Proof {
  type Challenge = Array[Byte]
}

trait Prover[SP <: SigmaProofOfKnowledgeProposition[_], P <: Proof[SP]] {
  val propCode: Byte

  def prove(sigmaProp: SP): P
}

trait Provers {
  val provers: Seq[Prover[_, _]]

  val supportedCodes = provers.map(_.propCode)


}

case class Or(statement1: SigmaStateProposition, statement2: SigmaStateProposition) extends SigmaStateProposition {
  override lazy val bytes: Array[Byte] = ???
}

case class And(statement1: SigmaStateProposition, statement2: SigmaStateProposition) extends SigmaStateProposition {
  override lazy val bytes: Array[Byte] = ???
}
