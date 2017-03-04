package sigmastate

import scapi.sigma.rework.NonInteractiveProver

trait Provers {
  val provers: Map[SigmaProposition.PropositionCode, Seq[NonInteractiveProver[_, _, _, _]]]

  lazy val supportedCodes = provers.keys.toSet

  def prove[P <: SigmaProposition](sigmaProp: P, message: Array[Byte]): Proof[P] = (sigmaProp match {
    case c: CAnd => CAndProof(c.props.map(p => prove(p, message)): _*)

    //   case COr() => ???
    case pokp: SigmaProofOfKnowledgeProposition[_, _] =>
      val prover = provers.getOrElse(pokp.code, Seq())
        .find(_.publicInput == pokp)
        .get

      prover.sign(message)
  }).asInstanceOf[Proof[P]]
}
