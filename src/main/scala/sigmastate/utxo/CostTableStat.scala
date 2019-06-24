package sigmastate.utxo

import scala.collection.mutable
import sigmastate.lang.Terms.OperationId

object CostTableStat {
  // NOTE: make immutable before making public
  private class StatItem(
    /** How many times the operation has been executed */
    var count: Long,
    /** Sum of all execution times */
    var sum: Long,
    /** Minimal length of the collection produced by the operation */
    var minLen: Int,
    /** Maximal length of the collection produced by the operation */
    var maxLen: Int,
    /** Sum of all lengths of the collections produced by the operation */
    var sumLen: Int
  )
  private val stat = mutable.HashMap[OperationId, StatItem]()
  def addOpTime(op: OperationId, time: Long, len: Int) = {
    stat.get(op) match {
      case Some(item) =>
        item.count += 1
        item.sum += time
        item.minLen = item.minLen min len
        item.maxLen = item.maxLen max len
        item.sumLen += len
      case None =>
        stat(op) = new StatItem(1, time, minLen = len, maxLen = len, sumLen = len)
    }
  }

  /** Prints the following string
    * Seq(
    * ("Const", "() => SByte", 1206), // count=199
    * ("GT", "(T,T) => SBoolean", 7954), // count=157
    * ("/", "(SByte,SByte) => SByte", 25180), // count=2
    * ("Inputs$", "(SContext) => Coll[SBox]", 4699), // count=443; minLen=0; maxLen=1000; avgLen=9
    * ("OptionIsDefined", "(Option[SSigmaProp]) => SBoolean", 9251), // count=2
    * )
    * */
  def costTableString: String = {
    stat.map { case (opId, item) =>
      val cost = item.sum / item.count
      val avgLen = item.sumLen / item.count
      val isColl = opId.opType.tRange.isCollection
      "\n" + s"""("${opId.name}", "${opId.opType}", $cost), // count=${item.count}${if (isColl) s"; minLen=${item.minLen}; maxLen=${item.maxLen}; avgLen=$avgLen" else ""}"""
    }.mkString("Seq(", "", "\n)")
  }
}
