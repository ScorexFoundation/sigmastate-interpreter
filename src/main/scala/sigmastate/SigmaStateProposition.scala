package sigmastate

import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.Proposition


trait SigmaStateProposition extends Proposition {
  override def serializer: Serializer[M] = ???
}

trait StateProposition extends SigmaStateProposition

trait SigmaProposition extends SigmaStateProposition {
  val code: SigmaProposition.PropositionCode
}

object SigmaProposition {
  type PropositionCode = Byte
}