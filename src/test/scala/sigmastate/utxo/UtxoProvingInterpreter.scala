package sigmastate.utxo

import scapi.sigma.rework.DLogProtocol.{DLogNode, DLogProverInput}
import scorex.utils.Random
import sigmastate.ByteArrayLeaf
import sigmastate.interpreter.ProverInterpreter
import sigmastate.utils.Helpers

class UtxoProvingInterpreter extends UtxoInterpreter with ProverInterpreter {
  implicit val dlog = DLogNode.dlogGroup
  implicit val soundness: Int = 256

  override lazy val secrets: Seq[DLogProverInput] = {
    (1 to 4).map(_ => DLogProverInput.random()._1)
  }

  override lazy val contextExtenders: Map[Int, ByteArrayLeaf] = (1 to 10).map { i =>
    val ba = Random.randomBytes(75)
    Helpers.tagInt(ba) -> ByteArrayLeaf(ba)
  }.toMap

  def withContextExtender(tag: Int, value: ByteArrayLeaf): UtxoProvingInterpreter = {
    val s = secrets
    val ce = contextExtenders

    new UtxoProvingInterpreter {
      override lazy val secrets: Seq[DLogProverInput] = s
      override lazy val contextExtenders: Map[Int, ByteArrayLeaf] = ce + (tag -> value)
    }
  }

  def withSecrets(additionalSecrets: Seq[DLogProverInput]): UtxoProvingInterpreter = {
    val ce = contextExtenders
    val s = secrets ++ additionalSecrets

    new UtxoProvingInterpreter {
      override lazy val secrets: Seq[DLogProverInput] = s
      override lazy val contextExtenders: Map[Int, ByteArrayLeaf] = ce
    }
  }
}
