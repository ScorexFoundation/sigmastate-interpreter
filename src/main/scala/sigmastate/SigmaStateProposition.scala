package sigmastate

import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.Proposition


trait SigmaStateProposition extends Proposition {
  override def serializer: Serializer[M] = ???

  //todo: remove after serialization, replace with just .bytes
  lazy val propBytes = this.toString.getBytes
}

trait StateProposition extends SigmaStateProposition

trait SigmaProposition extends SigmaStateProposition {
  val code: SigmaProposition.PropositionCode
}

object SigmaProposition {
  type PropositionCode = Byte
}