package sigmastate

import scorex.core.transaction.box.proposition.Proposition


trait SigmaStateProposition extends Proposition

trait StateProposition extends SigmaStateProposition

trait SigmaProposition extends SigmaStateProposition {
  val code: SigmaProposition.PropositionCode
}

object SigmaProposition {
  type PropositionCode = Byte
}