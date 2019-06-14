package sigmastate.eval

import org.ergoplatform.validation.ValidationRules
import sigmastate.SType
import sigmastate.Values.{Value, SValue, TrueSigmaProp}
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.TransformingSigmaBuilder
import sigmastate.interpreter.Interpreter
import sigmastate.lang.exceptions.CosterException

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
    def buildGraph(env: ScriptEnv, exp: SValue) = {
      val costed = buildCostedGraph[SType](env.map { case (k, v) => (k: Any, builder.liftAny(v).get) }, exp)
      asRep[Costed[Context] => Costed[Any]](costed)
    }
    val g = buildGraph(env, typed)
    val calcF = g.sliceCalc(okRemoveIsProven)
    val costF = g.sliceCostEx
    Pair(calcF, costF)
  }

  /** Can be overriden to to do for example logging or saving of graphs */
  private[sigmastate] def onCostingResult[T](env: ScriptEnv, tree: SValue, result: RCostingResultEx[T]) {
  }

  /** Can be overriden to to do for example logging of computed costs */
  private[sigmastate] def onEstimatedCost[T](env: ScriptEnv,
                                             tree: SValue,
                                             result: RCostingResultEx[T],
                                             ctx: special.sigma.Context,
                                             estimatedCost: Int): Unit = {
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

  /** TODO soft-fork: Version Based Costing
    * The following is based on ErgoTree.header checks performed during deserialization and
    * described in `ErgoTreeSerializer`
    * The next version should ensure that v2 node contained both old and new version of casting
    * component (Coster).
    *
    * Then v2 node during chain validation will use version in the ErgoTree header,
    * 1) if version = 1 then execute the old CosterV1
    * 2) if version = 2 then execute CosterV2.
    * Thus v2 protocol will apply the new costing for only new versions.
    *
    * With this scheme changing the parameters will not have negative effects,
    * the old scripts will be validated according to the old parameter values,
    * and the new ones according to the new values.
    *
    * And taking into account the cleaning of the garbage and cutting the blockchain history,
    * the old scripts at some point will die out of the blockchain.
    */
  def checkCostWithContext(ctx: SContext, exp: Value[SType],
                costF: Rep[((Context, (Int, Size[Context]))) => Int], maxCost: Long): Try[Int] = Try {
    val costFun = compile[(SContext, (Int, SSize[SContext])), Int, (Context, (Int, Size[Context])), Int](
                    getDataEnv, costF, Some(maxCost))
    val (_, estimatedCost) = costFun((ctx, (0, Sized.sizeOf(ctx))))
    if (estimatedCost > maxCost) {
      throw new CosterException(msgCostLimitError(estimatedCost, maxCost), None)
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

