package sigmastate.utxo

import sigmastate._
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import org.bitbucket.inkytonik.kiama.rewriting.Strategy

import scala.collection.mutable


case class UtxoContext(currentHeight: Long,
                       spendingTransaction: SigmaStateTransaction,
                       self: (SigmaStateBox, Long)) extends Context

trait UtxoVariable[V <: Value] extends Variable[V]

case object SelfHeight extends UtxoVariable[IntLeaf]
case object SelfAmount extends UtxoVariable[IntLeaf]
case object SelfScript extends UtxoVariable[PropLeaf]

case object OutputAmount extends Variable[IntLeaf]
case object OutputScript extends Variable[PropLeaf]

//todo: more strict-type solution Variable[V] => Value[V]
case class ScopedBinding(bindings: Map[Variable[_], Value], relations: Seq[Relation]) extends StateTree

trait Function extends StateTree

case class TxHasOutput(relation: Relation*) extends Function

class UtxoInterpreter extends Interpreter {
    override type StateT = StateTree
    override type CTX = UtxoContext

    override val maxDepth = 50

  def fnSubst(utxoContext: UtxoContext):Strategy = everywherebu {
    rule[SigmaStateTree] {
      case hasOut: TxHasOutput =>
        val ts = hasOut.relation.map { r =>
          (r.left, r.right) match {
            case (OutputAmount, _) | (_, OutputAmount) => OutputAmount -> r
            case (OutputScript, _) | (_, OutputScript) => OutputScript -> r
            case _ => ???
          }
        }

        val sbs = utxoContext.spendingTransaction.newBoxes.map { out =>
          val amount = out.value
          val bs = mutable.Map[Variable[_], Value]()

          val rs = ts.map { case (v, r) =>
            v match {
              case OutputAmount =>
                bs.put(OutputAmount, IntLeaf(amount))
                r
              case OutputScript =>
                bs.put(OutputScript, PropLeaf(out.proposition))
                r
              case _ => ???
            }
          }
          ScopedBinding(bs.toMap, rs)
        }
        OR(sbs.toSeq)
    }
  }

  def sbSubst():Strategy = everywherebu{
    rule[SigmaStateTree] {
      case sb: ScopedBinding =>
        val rels = sb.relations.map { r =>
          val rl = r.left match {
            case v: Variable[_] =>
              sb.bindings.get(v) match {
                case Some(value) => r.swapLeft(value)
                case None => r
              }
            case _ => r
          }

          rl.right match {
            case v: Variable[_] =>
              sb.bindings.get(v) match {
                case Some(value) => rl.swapRight(value)
                case None => rl
              }
            case _ => rl
          }
        }
        AND(rels)
    }
  }

  def ssSubst(context:UtxoContext) = everywherebu(rule[Value] {case SelfScript => PropLeaf(context.self._1.proposition)})

  def varSubst(context: UtxoContext): Strategy = everywherebu(
    rule[Value] {
    case Height => IntLeaf(context.currentHeight)
    case SelfHeight => IntLeaf(context.self._2)
    case SelfAmount => IntLeaf(context.self._1.value)
  })

  override def specificPhases(tree: SigmaStateTree, context: UtxoContext): SigmaStateTree = {

    val afterFn = fnSubst(context)(tree).get.asInstanceOf[SigmaStateTree]
    val afterSs = ssSubst(context)(afterFn).get.asInstanceOf[SigmaStateTree]
    val afterSb = sbSubst()(afterSs).get.asInstanceOf[SigmaStateTree]
    varSubst(context)(afterSb).get.asInstanceOf[SigmaStateTree]
  }
}
