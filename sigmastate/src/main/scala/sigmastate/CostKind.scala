package sigmastate

import scalan.OverloadHack.Overloaded1

/** Cost descriptor of a single operation, usually associated with
  * [[sigmastate.interpreter.OperationDesc]].
  */
sealed abstract class CostKind

/** Descriptor of the simple fixed cost.
  * @param cost  given cost of the operation */
case class FixedCost(cost: JitCost) extends CostKind
object FixedCost {
  def apply(jitCost: Int)(implicit o: Overloaded1): FixedCost = FixedCost(JitCost(jitCost))
}

/** Cost of operation over collection of the known length.
  * See for example [[Exists]], [[MapCollection]].
  * @param baseCost cost of operation factored out of the loop iterations
  * @param perChunkCost cost associated with each chunk of items
  * @param chunkSize number of items in a chunk
  */
case class PerItemCost(baseCost: JitCost, perChunkCost: JitCost, chunkSize: Int) extends CostKind {
  /** Compute number of chunks necessary to cover the given number of items. */
  def chunks(nItems: Int) = (nItems - 1) / chunkSize + 1

  /** Computes the cost for the given number of items. */
  def cost (nItems: Int): JitCost = {
    val nChunks = chunks(nItems)
    baseCost + (perChunkCost * nChunks)
  }
}
object PerItemCost {
  def apply(baseJitCost: Int, perChunkJitCost: Int, chunkSize: Int)
           (implicit o: Overloaded1): PerItemCost =
    PerItemCost(JitCost(baseJitCost), JitCost(perChunkJitCost), chunkSize)
}

/** Descriptor of the cost which depends on type. */
abstract class TypeBasedCost extends CostKind {
  /** Returns cost value depending on the given type. */
  def costFunc(tpe: SType): JitCost
}

/** Cost of operation cannot be described using fixed set of parameters.
  * In this case the operation cost is a sum of sub-operation costs.
  * See [[EQ]], [[NEQ]]. */
case object DynamicCost extends CostKind



