package sigmastate.utxo

import sigmastate.{BooleanConstantProposition, Interpreter, SigmaProposition}


abstract class UtxoBlockchainInterpreter(override val maxDepth: Int) extends Interpreter {
  override type CTX = UtxoBlockchainContext
  override type SProp = UtxoBlockchainProposition
  override type CProp = SigmaProposition

  override def statefulReductions[SP <: UtxoBlockchainProposition](proposition: SP,
                                                                   environment: UtxoBlockchainContext): BooleanConstantProposition = ???
}
