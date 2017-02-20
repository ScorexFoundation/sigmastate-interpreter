package sigmastate.utxo

import sigmastate._


case class TestingReducerInput(override val height: Int) extends BlockchainState


object TestingReducer extends Reducer {
  override type SProp = StateProposition
  override type Input = TestingReducerInput

  override val maxDepth = 50

  override def statefulReductions[SP <: StateProposition](proposition: SP, environment: TestingReducerInput): BooleanConstantProposition =
    proposition match {
      case HeightFromProposition(from) =>
        if (environment.height >= from) TrueProposition else FalseProposition
      case HeightBetweenProposition(from, until) =>
        if (environment.height >= from && environment.height < until) TrueProposition else FalseProposition
      case HeightUntilProposition(until) =>
        if (environment.height < until) TrueProposition else FalseProposition
    }
}