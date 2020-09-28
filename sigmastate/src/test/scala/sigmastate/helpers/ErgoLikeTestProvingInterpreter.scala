package sigmastate.helpers

import sigmastate.UnprovenConjecture
import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.basics.{DiffieHellmanTupleProverInput, SigmaProtocolPrivateInput}
import sigmastate.eval.IRContext
import sigmastate.interpreter.ProverInterpreter

class ErgoLikeTestProvingInterpreter(implicit IR: IRContext)
  extends ErgoLikeTestInterpreter with ProverInterpreter {

  override lazy val secrets: Seq[SigmaProtocolPrivateInput[_, _]] = {
    (1 to 4).map(_ => DLogProverInput.random()) ++
      (1 to 4).map(_ => DiffieHellmanTupleProverInput.random())
  }

  lazy val dlogSecrets: Seq[DLogProverInput] =
    secrets.filter(_.isInstanceOf[DLogProverInput]).asInstanceOf[Seq[DLogProverInput]]

  lazy val dhSecrets: Seq[DiffieHellmanTupleProverInput] =
    secrets.filter(_.isInstanceOf[DiffieHellmanTupleProverInput]).asInstanceOf[Seq[DiffieHellmanTupleProverInput]]

  // expose for testing
  override def setPositions(uc: UnprovenConjecture): UnprovenConjecture = super.setPositions(uc)
}
