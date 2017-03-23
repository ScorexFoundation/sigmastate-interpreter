package sigmastate.utxo

import scapi.sigma.rework.DLogProtocol.DLogProverInput
import sigmastate._
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._

case class TestingReducerInput(height: Int) extends Context


object TestingInterpreter extends Interpreter with DLogProverInterpreter {
  override type StateT = StateTree
  override type CTX = TestingReducerInput

  override val maxDepth = 50

  override def specificPhases(tree: SigmaStateTree, context: TestingReducerInput) = everywherebu(rule[Value] {
    case Height => IntLeaf(context.height)
  })(tree).get.asInstanceOf[SigmaStateTree]

  override lazy val secrets: Seq[DLogProverInput] = {
    import SchnorrSignature._

    Seq(DLogProverInput.random()._1, DLogProverInput.random()._1)
  }
}