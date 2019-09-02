package scalan.staged

import scala.collection.{mutable, _}
import scalan.{DFunc, Nullable, Scalan}
import scalan.compilation.GraphVizConfig
import scalan.util.GraphUtil
import spire.syntax.all.cfor
import debox.{Set => DSet, Buffer => DBuffer, Map => DMap}

import scala.annotation.tailrec

trait AstGraphs extends Transforming { self: Scalan =>

  /** GraphNode is created for each symbol of the AstGraph and represents graph linking structure */
  case class GraphNode(
          sym: Sym, // this symbol
          usages: DBuffer[Int]) {
    def inputSyms: Seq[Sym] = sym.node.deps
    def outSyms: DBuffer[Sym] = {
      usages.map(getSym)
    }
  }

  /** Type synonim for graph schedules. */
  type Schedule = Seq[Sym]

  /** Alternative representation of schedules using node ids. */
  type ScheduleIds = DBuffer[Int]

  /** Base class for all compound nodes with schedule (e.g. Lambda, ThunkDef).
    * The graph is directed acyclic (i.e. DAG) in which edges go from `boundVars` down to `roots`.
    */
  abstract class AstGraph extends Node { thisGraph =>
    /** If this graph represent Lambda abstraction, the boundVars is lambda bound symbols.
      * otherwise this is empty set. */
    def boundVars: Seq[Sym]

    /** Terminal nodes of the graph. There are incoming, but no outgoing edges. */
    def roots: Seq[Sym]

    /** Extract identifies out of `roots`. */
    def rootIds: DBuffer[Int] = {
      val rs = roots.toArray
      val len = rs.length
      val res = new Array[Int](len)
      cfor(0)(_ < len, _ + 1) { i => res(i) = rs(i).node.nodeId }
      DBuffer.unsafe(res)
    }

    /** Collect a set of symbols used in the graph but which are not part of its schedule.
      * If the graph represents a compound definition (Lambda, Thunk etc),
      * then each item in `freeVars` is used in the body, but not part of it.
      * Intersection of free vars with bound vars is empty.
      * @hotspot don't beautify the code
      */
    def freeVars: Seq[Sym] = {
      val sch = schedule.toArray
      val len = sch.length
      val resSet = DSet.ofSize[Int](len)
      cfor(0)(_ < len, _ + 1) { i =>
        val sym = sch(i)
        val deps = sym.node.deps
        cfor(0)(_ < deps.length, _ + 1) { j =>
          val s = deps(j)
          val sId = s.node.nodeId
          if (!resSet(sId)) {
            if (!(isLocalDefId(sId) || isBoundVar(s))) {
              resSet += sId
            }
          }
        }
      }
      val resIds = resSet.toArray()
      val res = new Array[Sym](resIds.length)
      cfor(0)(_ < resIds.length, _ + 1) { i =>
        res(i) = getSym(resIds(i))
      }
      res
    }

    /** Schedule represents a body of compound definition - topologically ordered
      * sequence of nodes of the graph. It is implemented differently depending
      * on node type.
      * @see Lambda, ThunkDef */
    def scheduleIds: DBuffer[Int]

    /** Sequence of node references forming a schedule.
      * @hotspot don't beautify the code */
    lazy val schedule: Schedule = {
      val len = scheduleIds.length
      val res = new Array[Sym](len)
      cfor(0)(_ < len, _ + 1) { i =>
        res(i) = getSym(scheduleIds(i))
      }
      res
    }

    /** Set of symbol ids in the schedule. Can be used to quickly recognize
      * symbols belonging to the body of this definition.
      */
    lazy val domain: DSet[Int] = {
      val res = DSet.ofSize[Int](scheduleIds.length)
      res ++= scheduleIds.toArray
      res
    }

    /** Whether this graph represents identity function. */
    @inline def isIdentity: Boolean

    /** Checks the symbol is lambda bound. */
    @inline def isBoundVar(s: Sym) = boundVars.contains(s)

    @inline final def isLocalDef(s: Sym): Boolean = domain(s.node.nodeId)
    @inline final def isLocalDefId(id: Int): Boolean = domain(id)

    @inline final def isRoot(s: Sym): Boolean = roots.contains(s)

    /** Flatten the given schedule into single sequence of non-AstGraph definitions.
      * All scope forming definitions like Lambda and ThunkDef are recursively unfolded in the given buffer `flatBuf`.
      * NOTE: The symbols of AstGraph-like definitions are added to `flatBuf` AFTER the unfolded body.
      */
    final def buildFlatSchedule(schedule: Schedule, flatBuf: DBuffer[Sym]): Unit = {
      cfor(0)(_ < schedule.length, _ + 1) { i =>
        val sym = schedule(i)
        if (sym.node.isInstanceOf[AstGraph]) {
          val subSch = sym.node.asInstanceOf[AstGraph].flatSchedule.toArray
          flatBuf ++= subSch
        }
        flatBuf += sym
      }
    }

    lazy val flatSchedule: Schedule = {
      val flatBuf = DBuffer.ofSize[Sym](schedule.length)
      buildFlatSchedule(schedule, flatBuf)
      flatBuf.toArray
    }

    /** Build usage information induced by the given schedule.
      * For each symbol of the schedule a GraphNode is created and usages are collected.
      * @hotspot don't beautify the code
      */
    def buildUsageMap(schedule: Schedule, usingDeps: Boolean): DMap[Int, GraphNode] = {
      val len = schedule.length
      val nodeMap = DMap.ofSize[Int, GraphNode](len)
      cfor(0)(_ < len, _ + 1) { i =>
        val sym = schedule(i)
        val symId = sym.node.nodeId
        nodeMap.update(symId, GraphNode(sym, DBuffer.empty[Int]))
      }
      cfor(0)(_ < len, _ + 1) { i =>
        val sym = schedule(i)
        val symId = sym.node.nodeId

        val deps = if (usingDeps) sym.node.deps else sym.node.syms
        cfor(0)(_ < deps.length, _ + 1) { j =>
          val us = deps(j)             // used symbol
          val usId = us.node.nodeId     // used symbol id
          var node = nodeMap.getOrElse(usId, null)
          if (null == node) {
            node = GraphNode(us, DBuffer.empty[Int])
            nodeMap.update(usId, node)
          }
          node.usages += symId
        }

      }
      nodeMap
    }

    /**
     * Symbol Usage information for this graph
     */
    lazy val usageMap: DMap[Int, GraphNode] = {
      buildUsageMap(schedule, usingDeps = true)
    }

    lazy val allNodes: DMap[Int, GraphNode] = {
      buildUsageMap(flatSchedule, usingDeps = false) // using rhs.syms instead of rhs.deps
    }

    def globalUsagesOf(s: Sym): DBuffer[Sym] = allNodes.get(s.node.nodeId) match {
      case Some(node) => node.outSyms
      case None => DBuffer.empty[Sym]
    }

    def hasManyUsagesGlobal(s: Sym): Boolean = globalUsagesOf(s).length > 1

    /** This empty buffer is returned every time the usages are requested for the node
      * that is not in usageMap.
      * WARNING! Since it is mutable, special care should be taken to not change this buffer.
      * @hotspot used havily in scheduling */
    private val NoUsages = DBuffer.unsafe(new Array[Int](0))

    /** @hotspot  for performance we return mutable structure, but it should never be changed. */
    def usagesOf(id: Int): DBuffer[Int] = {
      val node = usageMap.getOrElse(id, null)
      if (node == null) return NoUsages
      node.usages
    }

    def hasManyUsages(s: Sym): Boolean = usagesOf(s.node.nodeId).length > 1

    def show(): Unit = show(defaultGraphVizConfig)
    def show(emitMetadata: Boolean): Unit = show(defaultGraphVizConfig.copy(emitMetadata = emitMetadata))
    def show(config: GraphVizConfig): Unit = showGraphs(this)(config)

  } // AstGraph



}
