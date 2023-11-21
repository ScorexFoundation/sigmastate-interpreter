package sigma.interpreter.js

import sigma.data.{ProveDHTuple, WrapperOf}
import sigma.js.{Isos, SigmaProp}
import sigma.util.Extensions.BigIntOps
import sigmastate.crypto.DLogProtocol.DLogProverInput
import sigmastate.crypto.{DiffieHellmanTupleProverInput, SigmaProtocolPrivateInput}

import scala.scalajs.js

class ProverSecret(
  override val wrappedValue: SigmaProtocolPrivateInput[_]
) extends WrapperOf[SigmaProtocolPrivateInput[_]] {
}

object ProverSecret {
  def dlog(w: js.BigInt): ProverSecret = {
    val input = DLogProverInput(Isos.isoBigInt.to(w).toBigInteger)
    new ProverSecret(input)
  }
  def dht(w: js.BigInt, dhtProp: SigmaProp): ProverSecret = {
    dhtProp.sigmaBoolean match {
      case dht: ProveDHTuple =>
        val input = DiffieHellmanTupleProverInput(Isos.isoBigInt.to(w).toBigInteger, dht)
        new ProverSecret(input)
      case _ => throw new Exception("Expected ProveDHTuple sigma proposition")
    }
  }
}