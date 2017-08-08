package sigmastate.utxo

import scapi.sigma.DLogProtocol.{DLogNode, DLogProverInput}
import scapi.sigma.DiffieHellmanTupleProverInput
import scapi.sigma.rework.SigmaProtocolPrivateInput
import scorex.utils.Random
import sigmastate.ByteArrayLeaf
import sigmastate.interpreter.ProverInterpreter
import sigmastate.utils.Helpers

class UtxoProvingInterpreter(override val maxCost: Int = CostTable.ScriptLimit)
  extends UtxoInterpreter with ProverInterpreter {

  private implicit val dlog = DLogNode.dlogGroup
  private implicit val soundness: Int = 256

  override lazy val secrets: Seq[SigmaProtocolPrivateInput[_]] = {
    (1 to 4).map(_ => DLogProverInput.random()._1) ++
      (1 to 4).map(_ => DiffieHellmanTupleProverInput.random())
  }

  lazy val dlogSecrets: Seq[DLogProverInput] =
    secrets.filter(_.isInstanceOf[DLogProverInput]).asInstanceOf[Seq[DLogProverInput]]

  lazy val dhSecrets: Seq[DiffieHellmanTupleProverInput] =
    secrets.filter(_.isInstanceOf[DiffieHellmanTupleProverInput]).asInstanceOf[Seq[DiffieHellmanTupleProverInput]]

  override lazy val contextExtenders: Map[Int, ByteArrayLeaf] = (1 to 10).map { i =>
    val ba = Random.randomBytes(75)
    Helpers.tagInt(ba) -> ByteArrayLeaf(ba)
  }.toMap

  def withContextExtender(tag: Int, value: ByteArrayLeaf): UtxoProvingInterpreter = {
    val s = secrets
    val ce = contextExtenders

    new UtxoProvingInterpreter {
      override lazy val secrets = s
      override lazy val contextExtenders: Map[Int, ByteArrayLeaf] = ce + (tag -> value)
    }
  }

  def withSecrets(additionalSecrets: Seq[DLogProverInput]): UtxoProvingInterpreter = {
    val ce = contextExtenders
    val s = secrets ++ additionalSecrets

    new UtxoProvingInterpreter {
      override lazy val secrets = s
      override lazy val contextExtenders: Map[Int, ByteArrayLeaf] = ce
    }
  }
}
