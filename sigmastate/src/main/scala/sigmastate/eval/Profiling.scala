package sigmastate.eval

import sigmastate.SMethod
import sigmastate.Values.SValue
import sigmastate.lang.Terms
import sigmastate.serialization.OpCodes
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer.getSerializer
import scalan.util.Extensions.ByteOps
import scala.collection.mutable

class CostingRuleStat(val node: SValue, val outerStart: Long, var innerTime: Long, val outerEnd: Long)

class Profiler {
  private var ruleStack: List[CostingRuleStat] = Nil

  def onBeforeNode(node: SValue) = {
    val t = System.nanoTime()
    ruleStack = new CostingRuleStat(node, t, 0, t) :: ruleStack
  }

  def onAfterNode(node: SValue) = {
    val t = System.nanoTime()

    val rule = ruleStack.head   // always non empty at this point
    ruleStack = ruleStack.tail  // pop current rule
    assert(rule.node.opCode == node.opCode, s"Inconsistent stack at ${rule :: ruleStack}")

    val ruleFullTime = t - rule.outerStart  // full time spent in this rule

    // add this time to parent's innerTime (if any parent)
    if (ruleStack.nonEmpty) {
      val parent = ruleStack.head
      parent.innerTime += ruleFullTime
    } else {
      // top level
      //        println(s"Top time: $ruleFullTime")
    }

    val ruleSelfTime = ruleFullTime - rule.innerTime
    node match {
      case mc: Terms.MethodCall =>
        val m = mc.method
        addMcTime(m.objType.typeId, m.methodId, ruleSelfTime)
      case _ =>
        addOpTime(node.opCode, ruleSelfTime)
    }
  }

  // NOTE: this class is mutable so better to keep it private
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

