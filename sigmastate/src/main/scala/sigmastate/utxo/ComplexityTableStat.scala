package sigmastate.utxo

import sigmastate.serialization.ValueSerializer.getSerializer
import sigmastate.serialization.OpCodes.OpCode
import scalan.util.Extensions.ByteOps
import sigmastate.SMethod
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

  def addMcTime(typeId: Byte, methodId: Byte, time: Long) = {
    mcStat.get((typeId, methodId)) match {
      case Some(item) =>
        item.count += 1
        item.sum += time
      case None =>
        mcStat((typeId, methodId)) = new StatItem(1, time)
    }
  }

  /** Prints the complexity table
    * */
  def complexityTableString: String = {
    val opCodeLines = opStat.map { case (opCode, item) =>
      val avgTime = item.sum / item.count
      val time = avgTime / 1000
      val ser = getSerializer(opCode)
      val opName = ser.opDesc.typeName
      (opName, (opCode.toUByte - OpCodes.LastConstantCode).toString, time, item.count.toString)
    }.toList.sortBy(_._3)(Ordering[Long].reverse)

    val mcLines = mcStat.map { case (id @ (typeId, methodId), item) =>
      val avgTime = item.sum / item.count
      val time = avgTime / 1000
      val m = SMethod.fromIds(typeId, methodId)
      val typeName = m.objType.typeName
      (s"$typeName.${m.name}", typeId, methodId, time, item.count.toString)
    }.toList.sortBy(r => (r._2,r._3))(Ordering[(Byte,Byte)].reverse)

//    val lines = (("Op", "OpCode", "Avg Time,us", "Count") :: opCodeLines ::: mcLines)
//      .map { case (opName, opCode, time, count) =>
//        s"${opName.padTo(30, ' ')}\t${opCode.padTo(7, ' ')}\t${time.padTo(9, ' ')}\t${count}"
//      }
//      .mkString("\n")

    val rows = (opCodeLines)
      .map { case (opName, opCode, time, count) =>
        val key = s"$opName.opCode".padTo(30, ' ')
        s"$key -> $time,  // count = $count "
      }
      .mkString("\n")

    val mcRows = (mcLines)
        .map { case (opName, typeId, methodId, time, count) =>
          val key = s"($typeId.toByte, $methodId.toByte)".padTo(25, ' ')
          s"$key -> $time,  // count = $count, $opName "
        }
        .mkString("\n")

//    val total = opStat.values.foldLeft(0L) { (acc, item) => acc + item.sum }
    s"""
      |-----------
      |$rows
      |-----------
      |$mcRows
      |-----------
     """.stripMargin
  }
}
