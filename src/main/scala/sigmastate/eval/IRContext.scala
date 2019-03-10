package sigmastate.eval

import java.lang.reflect.Method

import sigmastate.SType
import sigmastate.Values.SValue
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.TransformingSigmaBuilder

trait IRContext extends Evaluation with TreeBuilding {
  import TestSigmaDslBuilder._

  override val builder = TransformingSigmaBuilder

  /** Pass configuration which is used to turn-off constant propagation.
    * @see `beginPass(noCostPropagationPass)`  */
  lazy val noConstPropagationPass = new DefaultPass(
    "noCostPropagationPass",
    Pass.defaultPassConfig.copy(constantPropagation = false))

  override val sigmaDslBuilderValue = CostingSigmaDslBuilder
  override val costedBuilderValue = sigmaDslBuilderValue.Costing
  override val monoidBuilderValue = sigmaDslBuilderValue.Monoids

  type RCostingResult[T] = Rep[(Context => T, ((Int, Size[Context])) => Int)]

  def doCosting[T](env: ScriptEnv, typed: SValue): RCostingResult[T] = {
    val costed = buildCostedGraph[SType](env.map { case (k, v) => (k: Any, builder.liftAny(v).get) }, typed)
    val f = asRep[Costed[Context] => Costed[T]](costed)
    val calcF = f.sliceCalc
    val costF = f.sliceCost
    Pair(calcF, costF)
  }

  /** Can be overriden to to do for example logging or saving of graphs */
  private[sigmastate] def onCostingResult[T](env: ScriptEnv, tree: SValue, result: RCostingResult[T]) {
  }
}

/** IR context to be used by blockchain nodes to validate transactions. */
class RuntimeIRContext extends IRContext with CompiletimeCosting {
//  override def isInvokeEnabled(d: Def[_], m: Method): Boolean = invokeAll
//  override def shouldUnpack(e: Elem[_]): Boolean = true
}

/** IR context to be used by script development tools to compile ErgoScript into ErgoTree bytecode. */
class CompiletimeIRContext extends IRContext with CompiletimeCosting {
//  override def invokeAll: Boolean = true
//  override def isInvokeEnabled(d: Def[_], m: Method): Boolean = invokeAll
//  override def shouldUnpack(e: Elem[_]): Boolean = true
}

