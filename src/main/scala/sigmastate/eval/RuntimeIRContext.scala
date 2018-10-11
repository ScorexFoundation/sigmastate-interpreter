package sigmastate.eval

import sigmastate.lang.TransformingSigmaBuilder

trait RuntimeIRContext extends Evaluation with TreeBuilding {
  import TestSigmaDslBuilder._

  override val sigmaDslBuilder = RTestSigmaDslBuilder()
  override val builder = TransformingSigmaBuilder

  beginPass(new DefaultPass("mypass", Pass.defaultPassConfig.copy(constantPropagation = false)))

  override val sigmaDslBuilderValue = new special.sigma.TestSigmaDslBuilder()
  override val costedBuilderValue = new special.collection.ConcreteCostedBuilder()
  override val monoidBuilderValue = new special.collection.MonoidBuilderInst()
}
