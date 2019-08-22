package sigmastate.eval

import java.lang.{Math => JMath}
import sigmastate.SType
import sigmastate.Values.{Value, SValue}
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.TransformingSigmaBuilder
import sigmastate.lang.exceptions.CostLimitException
import sigmastate.utxo.CostTable

import scala.util.Try

trait IRContext extends Evaluation with TreeBuilding {

  override val builder = TransformingSigmaBuilder

  /** Pass configuration which is used to turn-off constant propagation.
    * @see `beginPass(noCostPropagationPass)`  */
  lazy val noConstPropagationPass = new DefaultPass(
    "noCostPropagationPass",
    Pass.defaultPassConfig.copy(constantPropagation = false))

  override val sigmaDslBuilderValue = CostingSigmaDslBuilder
  override val costedBuilderValue = sigmaDslBuilderValue.Costing
  override val monoidBuilderValue = sigmaDslBuilderValue.Monoids

  type RCostingResult[T] = Ref[(Context => T, ((Int, Size[Context])) => Int)]

  case class RCostingResultEx[T](
    costedGraph: Ref[Costed[Context] => Costed[T]],
    costF: Ref[((Context, (Int, Size[Context]))) => Int]
  ) {
    lazy val calcF: Ref[Context => Any] = costedGraph.sliceCalc(true)
  }

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

  def doCostingEx(env: ScriptEnv,
                  typed: SValue,
                  okRemoveIsProven: Boolean): RCostingResultEx[Any] = {
    def buildGraph(env: ScriptEnv, exp: SValue) = {
      val costed = buildCostedGraph[SType](env.map { case (k, v) => (k: Any, builder.liftAny(v).get) }, exp)
      asRep[Costed[Context] => Costed[Any]](costed)
    }
    val g = buildGraph(env, typed)
    val costF = g.sliceCostEx
    RCostingResultEx(g, costF)
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

  /** Can be overriden to to do for example logging of computed results */
  private[sigmastate] def onResult[T](env: ScriptEnv,
                                      tree: SValue,
                                      result: RCostingResultEx[T],
                                      ctx: special.sigma.Context,
                                      estimatedCost: Int,
                                      calcCtx: special.sigma.Context,
                                      executedResult: special.sigma.SigmaProp,
                                      executionTime: Long): Unit = {
  }

  import Size._
  import Context._;

  def checkCost(ctx: SContext, exp: Value[SType],
                costF: Ref[Size[Context] => Int], maxCost: Long): Int = {
    val costFun = compile[SSize[SContext], Int, Size[Context], Int](getDataEnv, costF, Some(maxCost))
    val (_, estimatedCost) = costFun(Sized.sizeOf(ctx))
    if (estimatedCost > maxCost) {
      throw new CostLimitException(estimatedCost, s"Estimated execution cost $estimatedCost exceeds the limit $maxCost in $exp")
    }
    estimatedCost
  }

  def checkCostEx(ctx: SContext, exp: Value[SType],
                costF: Ref[((Int, Size[Context])) => Int], maxCost: Long): Int = {
    val costFun = compile[(Int, SSize[SContext]), Int, (Int, Size[Context]), Int](getDataEnv, costF, Some(maxCost))
    val (_, estimatedCost) = costFun((0, Sized.sizeOf(ctx)))
    if (estimatedCost > maxCost) {
      throw new CostLimitException(estimatedCost, s"Estimated execution cost $estimatedCost exceeds the limit $maxCost in $exp")
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
                costF: Ref[((Context, (Int, Size[Context]))) => Int], maxCost: Long, initCost: Long): Try[Int] = Try {
    val costFun = compile[(SContext, (Int, SSize[SContext])), Int, (Context, (Int, Size[Context])), Int](
                    getDataEnv, costF, Some(maxCost))
    val (estimatedCost, accCost) = costFun((ctx, (0, Sized.sizeOf(ctx))))

    if (debugModeSanityChecks) {
      if (estimatedCost != accCost)
        !!!(s"Estimated cost $estimatedCost should be equal $accCost")
    }

    val scaledCost = JMath.multiplyExact(estimatedCost.toLong, CostTable.costFactorIncrease.toLong) / CostTable.costFactorDecrease
    val totalCost = JMath.addExact(initCost, scaledCost)
    if (totalCost > maxCost) {
      throw new CostLimitException(totalCost, msgCostLimitError(totalCost, maxCost), None)
    }
    totalCost.toInt
  }

}

/** IR context to be used by blockchain nodes to validate transactions. */
class RuntimeIRContext extends IRContext {
}

/** IR context to be used by script development tools to compile ErgoScript into ErgoTree bytecode. */
class CompiletimeIRContext extends IRContext with CompiletimeCosting {
}

