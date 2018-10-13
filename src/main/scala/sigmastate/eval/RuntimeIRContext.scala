package sigmastate.eval

import sigmastate.SType
import sigmastate.Values.SValue
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.TransformingSigmaBuilder

trait RuntimeIRContext extends Evaluation with TreeBuilding {
  import TestSigmaDslBuilder._

  override val sigmaDslBuilder = RTestSigmaDslBuilder()
  override val builder = TransformingSigmaBuilder

  beginPass(new DefaultPass("mypass", Pass.defaultPassConfig.copy(constantPropagation = false)))

  override val sigmaDslBuilderValue = new special.sigma.TestSigmaDslBuilder()
  override val costedBuilderValue = new special.collection.ConcreteCostedBuilder()
  override val monoidBuilderValue = new special.collection.MonoidBuilderInst()

  type CostingResult[T] = Rep[(Context => T, Context => Int)]

  def doCosting(env: ScriptEnv, typed: SValue): CostingResult[Any] = {
    val costed = buildCostedGraph[SType](env.map { case (k, v) => (k: Any, builder.liftAny(v).get) }, typed)
    split2(asRep[Context => Costed[Any]](costed))
  }

  /** Can be overriden to to do for example logging or saving of graphs */
  private[sigmastate] def onCostingResult[T](env: ScriptEnv, tree: SValue, result: CostingResult[T]) {
  }
}
