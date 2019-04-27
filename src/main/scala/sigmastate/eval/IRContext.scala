package sigmastate.eval

import java.lang.reflect.Method

import sigmastate.SType
import sigmastate.Values.{Value, SValue}
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.TransformingSigmaBuilder

import scala.util.Try

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
  type RCostingResultEx[T] = Rep[(Context => T, ((Context, (Int, Size[Context]))) => Int)]

  def doCosting[T](env: ScriptEnv, typed: SValue): RCostingResult[T] = {
    val costed = buildCostedGraph[SType](env.map { case (k, v) => (k: Any, builder.liftAny(v).get) }, typed)
    val f = asRep[Costed[Context] => Costed[T]](costed)
    val calcF = f.sliceCalc
    val costF = f.sliceCost
    Pair(calcF, costF)
  }

  def doCosting(env: ScriptEnv, typed: SValue, okRemoveIsProven: Boolean): RCostingResult[Any] = {
    val costed = buildCostedGraph[SType](env.map { case (k, v) => (k: Any, builder.liftAny(v).get) }, typed)
    val f = asRep[Costed[Context] => Costed[Any]](costed)
    val calcF = f.sliceCalc(okRemoveIsProven)
    val costF = f.sliceCost
    Pair(calcF, costF)
  }

  def doCostingEx(env: ScriptEnv, typed: SValue, okRemoveIsProven: Boolean): RCostingResultEx[Any] = {
    val costed = buildCostedGraph[SType](env.map { case (k, v) => (k: Any, builder.liftAny(v).get) }, typed)
    val f = asRep[Costed[Context] => Costed[Any]](costed)
    val calcF = f.sliceCalc(okRemoveIsProven)
    val costF = f.sliceCostEx
    Pair(calcF, costF)
  }

  /** Can be overriden to to do for example logging or saving of graphs */
  private[sigmastate] def onCostingResult[T](env: ScriptEnv, tree: SValue, result: RCostingResultEx[T]) {
  }

  import Size._; import Context._;

  def checkCost(ctx: SContext, exp: Value[SType],
                costF: Rep[Size[Context] => Int], maxCost: Long): Int = {
    val costFun = compile[SSize[SContext], Int, Size[Context], Int](getDataEnv, costF, Some(maxCost))
    val (_, estimatedCost) = costFun(Sized.sizeOf(ctx))
    if (estimatedCost > maxCost) {
      throw new Error(s"Estimated expression complexity $estimatedCost exceeds the limit $maxCost in $exp")
    }
    estimatedCost
  }

  def checkCostEx(ctx: SContext, exp: Value[SType],
                costF: Rep[((Int, Size[Context])) => Int], maxCost: Long): Int = {
    val costFun = compile[(Int, SSize[SContext]), Int, (Int, Size[Context]), Int](getDataEnv, costF, Some(maxCost))
    val (_, estimatedCost) = costFun((0, Sized.sizeOf(ctx)))
    if (estimatedCost > maxCost) {
      throw new Error(s"Estimated expression complexity $estimatedCost exceeds the limit $maxCost in $exp")
    }
    estimatedCost
  }

  def checkCostWithContext(ctx: SContext, exp: Value[SType],
                costF: Rep[((Context, (Int, Size[Context]))) => Int], maxCost: Long): Try[Int] = Try {
    val costFun = compile[(SContext, (Int, SSize[SContext])), Int, (Context, (Int, Size[Context])), Int](
                    getDataEnv, costF, Some(maxCost))
    val (_, estimatedCost) = costFun((ctx, (0, Sized.sizeOf(ctx))))
    if (estimatedCost > maxCost) {
      throw new Error(msgCostLimitError(estimatedCost, maxCost))
    }
    estimatedCost
  }

}

/** IR context to be used by blockchain nodes to validate transactions. */
class RuntimeIRContext extends IRContext with CompiletimeCosting {
}

/** IR context to be used by script development tools to compile ErgoScript into ErgoTree bytecode. */
class CompiletimeIRContext extends IRContext with CompiletimeCosting {
}

