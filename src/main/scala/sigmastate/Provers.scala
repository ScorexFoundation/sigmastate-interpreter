package sigmastate

import org.bitbucket.inkytonik.kiama.attribution.Attribution
import scapi.sigma.rework.NonInteractiveProver


trait Provers extends Attribution {
  val provers: Map[SigmaProposition.PropositionCode, Seq[NonInteractiveProver[_, _, _, _]]]

  lazy val supportedCodes = (provers.keys.toSet ++ Set(CAND.Code)).ensuring(_.size == provers.size + 1)



  def prove(sigmaProp: SigmaTree, message: Array[Byte]): UncheckedTree[_] = ???

  /*
  {
    case c: CAND => CAndUncheckedNode(c, message, c.sigmaTrees.map(p => prove(p, message)):_*)

    //   todo: case COr() => ???

    case pokp: SigmaProofOfKnowledgeTree[_, _] =>
      val prover = provers.getOrElse(pokp.code, Seq())
        .find(_.publicInput == pokp)
        .get

      prover.sign(message)
  }
   */
}
