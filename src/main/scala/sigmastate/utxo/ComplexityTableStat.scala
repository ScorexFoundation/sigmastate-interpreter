package sigmastate.utxo

import sigmastate.serialization.ValueSerializer.getSerializer
import sigmastate.serialization.OpCodes.OpCode
import sigma.util.Extensions.ByteOps
import sigmastate.serialization.OpCodes

import scala.collection.mutable

object ComplexityTableStat {
  // NOTE: make immutable before making public
  private class StatItem(
    /** How many times the operation has been executed */
    var count: Long,
    /** Sum of all execution times */
    var sum: Long
  )

  /** Timings of op codes */
  private val opStat = mutable.HashMap[OpCode, StatItem]()

  /** Timings of method calls */
  private val mcStat = mutable.HashMap[(Byte, Byte), StatItem]()

  def addOpTime(op: OpCode, time: Long) = {
    opStat.get(op) match {
      case Some(item) =>
        item.count += 1
        item.sum += time
      case None =>
        opStat(op) = new StatItem(1, time)
    }
  }

  /** Prints the complexity table
    * */
  def complexityTableString: String = {
    val lines = (("Op", "OpCode", "Avg Time,us", "Count") :: (opStat.map { case (opCode, item) =>
      val avgTime = item.sum / item.count
      val timeStr = s"${avgTime / 1000}"
      val ser = getSerializer(opCode)
      val opName = ser.opDesc.typeName
      (opName, (opCode.toUByte - OpCodes.LastConstantCode).toString, timeStr, item.count.toString)
    }.toList))
        .sortBy(_._3)(Ordering[String].reverse)
    .map { case (opName, opCode, time, count) =>
      s"${opName.padTo(30, ' ')}\t${opCode.padTo(7, ' ')}\t${time.padTo(9, ' ')}\t${count}"
    } .mkString("\n")
    val total = opStat.values.foldLeft(0L) { (acc, item) => acc + item.sum }
    s"""
      |$lines
      |-----------
      |Total Time: $total
     """.stripMargin
  }
}
