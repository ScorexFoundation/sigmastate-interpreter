package sigmastate.utxo

import scapi.sigma.rework.DLogProtocol.DLogProverInput
import sigmastate._


case class TestingReducerInput(height: Int) extends Context


object TestingInterpreter extends Interpreter with DLogProverInterpreter {
  override type SProp = StateProposition
  override type CTX = TestingReducerInput

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

  override lazy val secrets: Seq[DLogProverInput] = {
    import SchnorrSignature._

    Seq(DLogProverInput.random()._1, DLogProverInput.random()._1)
  }
}