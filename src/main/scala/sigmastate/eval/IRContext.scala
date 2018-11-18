package sigmastate.eval

import sigmastate.SType
import sigmastate.Values.SValue
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.TransformingSigmaBuilder

trait IRContext extends Evaluation with TreeBuilding {
  import TestSigmaDslBuilder._

  override val builder = TransformingSigmaBuilder

  beginPass(new DefaultPass("mypass", Pass.defaultPassConfig.copy(constantPropagation = false)))

  override val sigmaDslBuilderValue = new CostingSigmaDslBuilder(this)
  override val costedBuilderValue = new special.collection.CCostedBuilder()
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

/** IR context to be used by blockchain nodes to validate transactions. */
class RuntimeIRContext extends IRContext

/** IR context to be used by script development tools to compile ErgoScript into ErgoTree bytecode. */
class CompiletimeIRContext extends IRContext

