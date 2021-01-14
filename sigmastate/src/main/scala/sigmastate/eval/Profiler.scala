package sigmastate.eval

import sigmastate.SMethod
import sigmastate.Values.SValue
import sigmastate.lang.Terms
import sigmastate.serialization.OpCodes
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer.getSerializer
import scalan.util.Extensions.ByteOps
import debox.{Buffer => DBuffer, Map => DMap}
import sigmastate.interpreter.{CostItem, SeqCostItem, PerBlockCostItem}

/** A simple profiler to measure average execution times of ErgoTree operations. */
class Profiler {

  // NOTE: this class is mutable so better to keep it private
  private class OpStat(
    /** The node on the evaluation stack. */
    val node: SValue,
    /** The time then this node evaluation was started. */
    val outerStart: Long,
    /** The accumulated time of evaluating all the sub-nodes */
    var innerTime: Long,
    /** The time then this nodes evaluation finished */
    val outerEnd: Long
  )

  /** If every recursive evaluation of every Value is marked with
    * [[onBeforeNode()]]/[[onAfterNode()]], then this stack corresponds to the stack of
    * recursive invocations of the evaluator. */
  private var opStack: List[OpStat] = Nil

  /** Called from evaluator (like [[sigmastate.interpreter.ErgoTreeEvaluator]])
    * immediately before the evaluator start recursive evaluation of the given node.
    */
  def onBeforeNode(node: SValue) = {
    val t = System.nanoTime()
    opStack = new OpStat(node, t, 0, t) :: opStack
  }

  /** Called from evaluator (like [[sigmastate.interpreter.ErgoTreeEvaluator]])
    * immediately after the evaluator finishes recursive evaluation of the given node.
    */
  def onAfterNode(node: SValue) = {
    val t = System.nanoTime()

    val op = opStack.head   // always non empty at this point
    opStack = opStack.tail  // pop current op
    assert(op.node.opCode == node.opCode, s"Inconsistent stack at ${op :: opStack}")

    val opFullTime = t - op.outerStart  // full time spent in this op

    // add this time to parent's innerTime (if any parent)
    if (opStack.nonEmpty) {
      val parent = opStack.head
      parent.innerTime += opFullTime
    } else {
      // we are on top level, do nothing
    }

    val opSelfTime = opFullTime - op.innerTime

    // update timing stats
    node match {
      case mc: Terms.MethodCall =>
        val m = mc.method
        addMcTime(m.objType.typeId, m.methodId, opSelfTime)
      case _ =>
        addOpTime(node.opCode, opSelfTime)
    }
  }

  // NOTE: this class is mutable so better to keep it private
  private class StatItem {
      val times: DBuffer[Long] = DBuffer.ofSize(256)

      def addTime(time: Long) = times += time

      /** How many times the operation has been executed */
      def count: Int = times.length

      /** Sum of all execution times */
      def sum: Long = times.sum(spire.math.Numeric[Long])

      /** Returns average time in nanoseconds. */
      def avgTimeNano: Long = sum / count

      /** Returns average time in microseconds. */
      def avgTimeMicroseconds: Long = {
        val avgTime = sum / count
        avgTime / 1000
      }
  }


  /** Timings of op codes. For performance debox implementation of Map is used. */
  private val opStat = DMap[OpCode, StatItem]()

  /** Update time measurement stats for a given operation. */
  @inline private final def addOpTime(op: OpCode, time: Long) = {
    val item = opStat.getOrElse(op, null)
    if (item != null) {
      item.addTime(time)
    } else {
      val item = new StatItem
      item.addTime(time)
      opStat(op) = item
    }
  }

  /** Timings of method calls */
  private val mcStat = DMap[(Byte, Byte), StatItem]()  // TODO JITC, TODO optimize: pack (Byte, Byte) into Short

  /** Update time measurement stats for a given method. */
  @inline private final def addMcTime(typeId: Byte, methodId: Byte, time: Long) = {
    val key = (typeId, methodId)
    val item = mcStat.getOrElse(key, null)
    if (item != null) {
      item.addTime(time)
    } else {
      val item = new StatItem
      item.addTime(time)
      mcStat(key) = item
    }
  }

  /** Timings of cost items */
  private val costItemsStat = DMap[CostItem, StatItem]()

  def addCostItem(costItem: CostItem, time: Long) = {
    val item = costItemsStat.getOrElse(costItem, null)
    if (item != null) {
      item.addTime(time)
    } else {
      val item = new StatItem
      item.addTime(time)
      costItemsStat(costItem) = item
    }
  }

  /** Prints the operation timing table using collected information.
    */
  def opStatTableString: String = {
    val opCodeLines = opStat.mapToArray { case (opCode, stat) =>
      val time = stat.avgTimeMicroseconds
      val ser = getSerializer(opCode)
      val opName = ser.opDesc.typeName
      (opName, (opCode.toUByte - OpCodes.LastConstantCode).toString, time, stat.count.toString)
    }.toList.sortBy(_._3)(Ordering[Long].reverse)

    val mcLines = mcStat.mapToArray { case (id @ (typeId, methodId), stat) =>
      val time = stat.avgTimeMicroseconds
      val m = SMethod.fromIds(typeId, methodId)
      val typeName = m.objType.typeName
      (s"$typeName.${m.name}", typeId, methodId, time, stat.count.toString)
    }.toList.sortBy(r => (r._2,r._3))(Ordering[(Byte,Byte)].reverse)

    val ciLines = costItemsStat.mapToArray { case (ci, stat) =>
      val time = ci match {
        case SeqCostItem(_, _, nItems) => stat.avgTimeNano / nItems
        case PerBlockCostItem(_, _, nBlocks) => stat.avgTimeNano / nBlocks
      }
      (ci.toString, time, stat.count.toString)
    }.toList.sortBy(_._2)(Ordering[Long].reverse)


    val rows = opCodeLines
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

    val ciRows = (ciLines)
        .map { case (opName, time, count) =>
          val key = s"$opName".padTo(30, ' ')
          s"$key -> $time,  // count = $count "
        }
        .mkString("\n")

    s"""
      |-----------
      |$rows
      |-----------
      |$mcRows
      |-----------
      |$ciRows
      |-----------
     """.stripMargin
  }

}

