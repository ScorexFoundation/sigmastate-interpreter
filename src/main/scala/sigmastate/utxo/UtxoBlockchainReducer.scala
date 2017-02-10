package sigmastate.utxo

import sigmastate.{BooleanConstantProposition, Reducer, SigmaProposition}


abstract class UtxoBlockchainReducer(override val maxDepth: Int) extends Reducer {
  override type Input = UtxoBlockchainState
  override type SProp = UtxoBlockchainProposition
  override type CProp = SigmaProposition

  override def statefulReductions[SP <: UtxoBlockchainProposition](proposition: SP,
                                                                   environment: UtxoBlockchainState): BooleanConstantProposition = ???
}
