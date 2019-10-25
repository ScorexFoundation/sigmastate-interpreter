package scalan.staged

import scalan.{DFunc, Nullable, Scalan}
import debox.{Buffer => DBuffer}
import scalan.util.GraphUtil
import spire.syntax.all.cfor

import scala.collection.mutable

trait ProgramGraphs extends AstGraphs { self: Scalan =>

  type PGraph = ProgramGraph

  /** Deboxed function to obtain usages of a given node.
    * Represents adjacency matrix of the reversed graph `g`.
    * @param  g  original graph whose usages are computed */
  class PGraphUsages(g: AstGraph) extends DFunc[Int, DBuffer[Int]] {
    override def apply(nodeId: Int) = {
      val us = g.usagesOf(nodeId)
      us
    }
  }

  /** Immutable graph collected from `roots` following Ref.node.deps links. */
  case class ProgramGraph(roots: Seq[Sym], mapping: Nullable[Transformer], filterNode: Nullable[Sym => Boolean])
  	  extends AstGraph {
    def this(roots: Seq[Sym], filterNode: Nullable[Sym => Boolean] = Nullable.None) { this(roots, Nullable.None, filterNode) }
    def this(root: Sym) { this(List(root)) }

    override lazy val rootIds: DBuffer[Int] = super.rootIds

    override def boundVars = Nil
    override def isIdentity: Boolean = false
    override def freeVars = mutable.WrappedArray.empty[Sym]
    override lazy val scheduleIds = {
      val neighbours: DFunc[Int, DBuffer[Int]] = filterNode match {
        case Nullable(pred) =>
          { (id: Int) =>
            val deps = getSym(id).node.deps
            val len = deps.length
            val res = DBuffer.ofSize[Int](len)
            cfor(0)(_ < len, _ + 1) { i =>
              val sym = deps(i)
              if (pred(sym) && !sym.isVar)  // TODO remove isVar condition here and below
                res += sym.node.nodeId
            }
            res
          }
        case _ =>
          { (id: Int) =>
            val deps = getSym(id).node.deps
            val len = deps.length
            val res = DBuffer.ofSize[Int](len)
            cfor(0)(_ < len, _ + 1) { i =>
              val sym = deps(i)
              if (!sym.isVar)
                res += sym.node.nodeId
            }
            res
          }
      }
      val sch = GraphUtil.depthFirstOrderFrom(rootIds, neighbours)
      sch
    }

    /** Mirror all the nodes of this graph applying transformer and performing rewriting.
      * @param m   mirror instance to be used for mirroring of nodes
      * @param rw  rewriter to be tried for each new created mirrored node
      * @param t   transformer of symbols, to be used for substitution of symbols in the new nodes.
      * @return   new graph which is not necessary clone of this graph, but should be semantically
      *           equivalent to this graph (provided all rw rules preserve equivalence).
      *           If rw is identity, then the resulting graph is alpha-equivalent to this graph
      *           as long as t is bijection.
      */
    def transform(m: Mirror, rw: Rewriter, t: Transformer): ProgramGraph = {
      val t0 = mapping match {
        case Nullable(mapping) => t merge mapping
        case _ => t
      }
      val t1 = m.mirrorSymbols(t0, rw, this, scheduleIds)
      val newRoots = roots map { t1(_) }
      new ProgramGraph(newRoots, Nullable(t1), filterNode)
    }

    /** Remove transformer component of the graph. */
    def withoutContext = ProgramGraph(roots, Nullable.None, filterNode)

    override def toString: String = {
      val mappingStr = if (mapping.isEmpty) "None" else mapping.toString
      val filterNodeStr = if (filterNode.isDefined) filterNode.toString else "None"
      s"ProgramGraph($roots, $mappingStr, $filterNodeStr)"
    }
  }

  object ProgramGraph {
    def transform[A](s: Ref[A], rw: Rewriter = NoRewriting, t: MapTransformer = MapTransformer.empty()): Ref[A] = {
      val g = ProgramGraph(List(s), Nullable.None, Nullable.None)
      val g1 = g.transform(DefaultMirror, rw, t)
      g1.roots(0).asInstanceOf[Ref[A]]
    }
  }
}
