package sigmastate.eval

import sigmastate.SMethod
import sigmastate.Values.{SValue, FixedCost}
import sigmastate.lang.Terms
import sigmastate.serialization.OpCodes
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer.getSerializer
import scalan.util.Extensions.ByteOps
import debox.{Buffer => DBuffer, Map => DMap}
import sigmastate.interpreter.{PerBlockCostItem, SeqCostItem, CostItem, TypeBasedCostItem, FixedCostItem}
import sigmastate.lang.Terms.{PropertyCall, MethodCall}
import spire.{math, sp}
import scalan.util.PrintExtensions._

import scala.reflect.ClassTag

abstract class StatItem[@sp (Long, Double) V] {
  /** How many data points has been collected */
  def count: Int

  /** Sum of all data points */
  def sum: V

  /** Returns arithmetic average value. */
  def avg: V

  /** Returns arithmetic mean value (excluding 10% of smallest and 10% of highest values).
    */
  def mean: (V, Int)
}

class StatCollection[@sp(Int) K, @sp(Long, Double) V]
  (implicit n: math.Numeric[V], ctK: ClassTag[K], ctV: ClassTag[V]) {

  private def calcAvg(buf: DBuffer[V]): V = {
    n.div(buf.sum, n.fromInt(buf.length))
  }

  // NOTE: this class is mutable so better to keep it private
  class StatItemImpl extends StatItem[V] {
    final val NumMaxPoints = 20000

    val dataPoints: DBuffer[V] = DBuffer.ofSize[V](256)

    def addPoint(point: V) = {
      if (dataPoints.length < NumMaxPoints) {
        dataPoints += point
      }
    }

    override def count: Int = dataPoints.length
    override def sum: V = dataPoints.sum
    override def avg: V = calcAvg(dataPoints)

    override def mean: (V, Int) = {
      val nCropped = dataPoints.length / 10
      if (nCropped == 0) {
        (calcAvg(dataPoints), dataPoints.length)
      }
      else {
        val sorted = dataPoints.copy()
        sorted.sort
        val slice = sorted.slice(nCropped, sorted.length - nCropped)
        (calcAvg(slice), slice.length)
      }
    }
  }

  /** Timings of op codes. For performance debox.Map is used, which keeps keys unboxed. */
  private val opStat = DMap[K, StatItemImpl]()

  final def getMean(key: K): Option[(V, Int)] = opStat.get(key).map(_.mean)

  /** Update time measurement stats for a given operation. */
  final def addPoint(key: K, point: V) = {
    val item = opStat.getOrElse(key, null)
    if (item != null) {
      item.addPoint(point)
    } else {
      val item = new StatItemImpl
      item.addPoint(point)
      opStat(key) = item
    }
  }

  final def mapToArray[@sp C: ClassTag](f: (K, StatItem[V]) => C): Array[C] = {
    opStat.mapToArray(f)
  }
}

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
    addOpTime(node.opCode, opSelfTime)
  }

  /** Timings of op codes. For performance debox implementation of Map is used. */
  private val opStat = new StatCollection[Int, Long]()

  /** Update time measurement stats for a given operation. */
  @inline private final def addOpTime(op: OpCode, time: Long) = {
    opStat.addPoint(OpCode.raw(op), time)
  }

  /** Timings of method calls */
  private val mcStat = new StatCollection[Int, Long]()

  /** Update time measurement stats for a given method. */
  @inline private final def addMcTime(typeId: Byte, methodId: Byte, time: Long) = {
    val key = typeId << 8 | methodId
    mcStat.addPoint(key, time)
  }

  /** Timings of cost items */
  private val costItemsStat = new StatCollection[CostItem, Long]()

  def addCostItem(costItem: CostItem, time: Long) = {
    costItemsStat.addPoint(costItem, time)
  }

  /** Estimation cost for each script */
  private val estimationCostStat = new StatCollection[String, Int]()
  /** Estimation cost for each script */
  private val measuredTimeStat = new StatCollection[String, Long]()

  /** Returns relative error between estimated and actual values. */
  def relativeError(est: Double, act: Double): Double = {
    val delta = Math.abs(est - act)
    delta / act
  }

  /** Adds estimated cost and actual measured time data point to the StatCollection for
    * the given script.
    */
  def addEstimation(script: String, cost: Int, actualTimeNano: Long) = {
    estimationCostStat.addPoint(script, cost)
    measuredTimeStat.addPoint(script, actualTimeNano)
  }

  def suggestCost(time: Long): Int = {
    ((time - 1) / 100 + 1).toInt
  }

  /** Prints the operation timing table using collected execution profile information.
    */
  def generateReport(): String = {
    val opCodeLines = opStat.mapToArray { case (key, stat) =>
      val (time, count) = stat.mean
      val opCode = OpCode @@ key.toByte
      val ser = getSerializer(opCode)
      val opDesc = ser.opDesc
      val (opName, cost) = opDesc.costKind match {
        case FixedCost(c) if opDesc != MethodCall && opDesc != PropertyCall =>
          (opDesc.typeName, c)
        case _ => ("", 0)
      }
      val suggestedCost = suggestCost(time)
      val comment = s"count: $count, suggestedCost: $suggestedCost, actualCost: $cost"
      (opName, (opCode.toUByte - OpCodes.LastConstantCode).toString, time, comment)
    }.filter(_._1.nonEmpty).sortBy(_._3)(Ordering[Long].reverse)

    val mcLines = mcStat.mapToArray { case (key, stat) =>
      val methodId = (key & 0xFF).toByte
      val typeId = (key >> 8).toByte
      val (time, count) = stat.mean
      val m = SMethod.fromIds(typeId, methodId)
      val typeName = m.objType.typeName
      (s"$typeName.${m.name}", typeId, methodId, time, count.toString)
    }.sortBy(r => (r._2,r._3))(Ordering[(Byte,Byte)].reverse)

    val ciLines = costItemsStat.mapToArray { case (ci, stat) =>
      val (name, timePerItem, time, comment) = {
        val (time, count) = stat.mean
        ci match {
          case ci: FixedCostItem =>
            val suggestedCost = suggestCost(time)
            val comment = s"count: $count, suggestedCost: $suggestedCost, actualCost: ${ci.cost}"
            (ci.opName, time, time, comment)
          case ci: TypeBasedCostItem =>
            val suggestedCost = suggestCost(time)
            val comment = s"count: $count, suggestedCost: $suggestedCost, actualCost: ${ci.cost}"
            (ci.opName, time, time, comment)
          case ci @ SeqCostItem(_, costKind, nItems) =>
            val nChunks = ci.chunks
            val timePerChunk = if (nChunks > 0) time / nChunks else time
            val name = s"${ci.opName}(nItems: $nItems, nChunks: $nChunks)"
            val comment = s"count: $count, costKind: $costKind"
            (name, timePerChunk, time, comment)
          case PerBlockCostItem(_, perBlockCost, nBlocks) =>
            val timePerBlock = if (nBlocks > 0) time / nBlocks else time
            val name = s"${ci.opName}(nBlocks: $nBlocks)"
            val comment = s"count: $count, perBlockCost: $perBlockCost"
            (name, timePerBlock, time, comment)
        }
      }
      (name, timePerItem, time, comment)
    }.sortBy({ case (name, tpi, t, c) => (name, tpi)})(Ordering[(String, Long)])

    val estLines = estimationCostStat.mapToArray { case (script, stat) =>
      val (cost, count) = stat.mean
      val (timeNano, _) = measuredTimeStat.getMean(script).get
      val actualTimeMicro = timeNano.toDouble / 100
      val actualCost = cost.toDouble
      val error = relativeError(actualCost, actualTimeMicro)
      (script, error, cost, timeNano, count.toString)
    }.sortBy(_._2)(Ordering[Double].reverse)


    val rows = opCodeLines
        .map { case (opName, opCode, time, comment) =>
          val key = s"$opName".padTo(26, ' ')
          s"$key -> time: $time ns, $comment "
        }
        .mkString("\n")

    val mcRows = mcLines
        .map { case (opName, typeId, methodId, time, count) =>
          val key = s"($typeId.toByte, $methodId.toByte)".padTo(25, ' ')
          s"$key -> $time,  // count = $count, $opName "
        }
        .mkString("\n")

    val ciRows = ciLines
        .map { case (opName, timePerItem, time, comment) =>
          val key = s"$opName".padTo(40, ' ')
          val totalTime = if (time != timePerItem) s"($time)" else ""
          s"$key -> $timePerItem${totalTime} ns, $comment"
        }
        .mkString("\n")

    val estRows = estLines
        .map { case (opName, error, cost, time, count) =>
          val key = s"$opName".padTo(30, ' ')
          s"$key -> ($error, $cost, $time),  // count = $count "
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
      |$estRows
      |-----------
     """.stripMargin
  }

}

