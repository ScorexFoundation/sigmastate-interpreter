package sigmastate.helpers

import scapi.sigma.DLogProtocol.DLogProverInput
import scapi.sigma.{SigmaProtocolPrivateInput, DiffieHellmanTupleProverInput}
import scorex.utils.Random
import sigmastate.SType
import sigmastate.Values._
import sigmastate.eval.IRContext
import sigmastate.interpreter.ProverInterpreter
import sigmastate.utxo.{CostTable, ErgoLikeTestInterpreter}

class ErgoLikeTestProvingInterpreter(override val maxCost: Long = CostTable.ScriptLimit)(implicit override val IR: IRContext)
  extends ErgoLikeTestInterpreter(maxCost) with ProverInterpreter {

  override lazy val secrets: Seq[SigmaProtocolPrivateInput[_, _]] = {
    (1 to 4).map(_ => DLogProverInput.random()) ++
      (1 to 4).map(_ => DiffieHellmanTupleProverInput.random())
  }

  lazy val dlogSecrets: Seq[DLogProverInput] =
    secrets.filter(_.isInstanceOf[DLogProverInput]).asInstanceOf[Seq[DLogProverInput]]

  lazy val dhSecrets: Seq[DiffieHellmanTupleProverInput] =
    secrets.filter(_.isInstanceOf[DiffieHellmanTupleProverInput]).asInstanceOf[Seq[DiffieHellmanTupleProverInput]]

  override lazy val contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] = (1 to 10).map { i =>
    val ba = Random.randomBytes(75)
    i.toByte -> ByteArrayConstant(ba)
  }.toMap

  def withContextExtender(tag: Byte, value: EvaluatedValue[_ <: SType]): ErgoLikeTestProvingInterpreter = {
    val s = secrets
    val ce = contextExtenders

    new ErgoLikeTestProvingInterpreter(maxCost) {
      override lazy val secrets = s
      override lazy val contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] = ce + (tag -> value)
    }
  }

  def withSecrets(additionalSecrets: Seq[DLogProverInput]): ErgoLikeTestProvingInterpreter = {
    val ce = contextExtenders
    val s = secrets ++ additionalSecrets

    new ErgoLikeTestProvingInterpreter(maxCost) {
      override lazy val secrets = s
      override lazy val contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] = ce
    }
  }
}
