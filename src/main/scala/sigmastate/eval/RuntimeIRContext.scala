package sigmastate.eval

import sigmastate.SType
import sigmastate.Values.SValue
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

  def doCosting[T](env: Map[String, Any], typed: SValue): CostingResult[T] = {
    val costed = buildCostedGraph[SType](env.mapValues(builder.liftAny(_).get), typed)
    split2(costed.asRep[Context => Costed[T]])
  }

  /** Can be overriden to to do for example logging or saving of graphs */
  private[sigmastate] def onCostingResult[T](result: CostingResult[T]) {
  }
}
