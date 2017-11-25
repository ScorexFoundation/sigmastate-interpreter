package sigmastate.utxo

import scapi.sigma.DLogProtocol.{ProveDlog, DLogProverInput}
import scapi.sigma.DiffieHellmanTupleProverInput
import scapi.sigma.rework.SigmaProtocolPrivateInput
import scorex.utils.Random
import sigmastate.ByteArrayLeafConstant
import sigmastate.interpreter.ProverInterpreter
import sigmastate.utils.Helpers

class UtxoProvingInterpreter(override val maxCost: Int = CostTable.ScriptLimit)
  extends UtxoInterpreter(maxCost) with ProverInterpreter {

  private implicit val dlog = ProveDlog.dlogGroup
  private implicit val soundness: Int = 256

  override lazy val secrets: Seq[SigmaProtocolPrivateInput[_]] = {
    (1 to 4).map(_ => DLogProverInput.random()._1) ++
      (1 to 4).map(_ => DiffieHellmanTupleProverInput.random())
  }

  lazy val dlogSecrets: Seq[DLogProverInput] =
    secrets.filter(_.isInstanceOf[DLogProverInput]).asInstanceOf[Seq[DLogProverInput]]

  lazy val dhSecrets: Seq[DiffieHellmanTupleProverInput] =
    secrets.filter(_.isInstanceOf[DiffieHellmanTupleProverInput]).asInstanceOf[Seq[DiffieHellmanTupleProverInput]]

  override lazy val contextExtenders: Map[Int, ByteArrayLeafConstant] = (1 to 10).map { i =>
    val ba = Random.randomBytes(75)
    Helpers.tagInt(ba) -> ByteArrayLeafConstant(ba)
  }.toMap

  def withContextExtender(tag: Int, value: ByteArrayLeafConstant): UtxoProvingInterpreter = {
    val s = secrets
    val ce = contextExtenders

    new UtxoProvingInterpreter(maxCost) {
      override lazy val secrets = s
      override lazy val contextExtenders: Map[Int, ByteArrayLeafConstant] = ce + (tag -> value)
    }
  }

  def withSecrets(additionalSecrets: Seq[DLogProverInput]): UtxoProvingInterpreter = {
    val ce = contextExtenders
    val s = secrets ++ additionalSecrets

    new UtxoProvingInterpreter(maxCost) {
      override lazy val secrets = s
      override lazy val contextExtenders: Map[Int, ByteArrayLeafConstant] = ce
    }
  }
}
