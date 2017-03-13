package sigmastate

import scapi.sigma.rework.NonInteractiveProver
import sigmastate.experimental.{CAndUnprovenNode, UnprovenTree}

trait Provers {
  val provers: Map[SigmaProposition.PropositionCode, Seq[NonInteractiveProver[_, _, _, _]]]

  lazy val supportedCodes = (provers.keys.toSet ++ Set(CAnd.Code)).ensuring(_.size == provers.size + 1)

  def prove[P <: SigmaProposition](sigmaProp: P, message: Array[Byte]): UnprovenTree[P] = (sigmaProp match {
    case c: CAnd => CAndUnprovenNode(c, message, c.props.map(p => prove(p, message)):_*)

    //   case COr() => ???
    case pokp: SigmaProofOfKnowledgeProposition[_, _] =>
      val prover = provers.getOrElse(pokp.code, Seq())
        .find(_.publicInput == pokp)
        .get

      prover.sign(message)
  }).asInstanceOf[UnprovenTree[P]]
}
