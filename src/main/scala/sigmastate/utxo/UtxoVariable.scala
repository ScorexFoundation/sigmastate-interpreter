package sigmastate.utxo

import sigmastate._
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import scapi.sigma.rework.DLogProtocol.{DLogNode, DLogProverInput}

import scala.collection.mutable


case class UtxoContext(currentHeight: Long,
                       spendingTransaction: SigmaStateTransaction,
                       self: SigmaStateBox) extends Context

trait UtxoVariable[V <: Value] extends Variable[V]

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

  def varSubst(context: UtxoContext): Strategy = everywherebu(
    rule[Value] {
    case Height => IntLeaf(context.currentHeight)
  })

  override def specificPhases(tree: SigmaStateTree, context: UtxoContext): SigmaStateTree = {
    val afterFn = fnSubst(context)(tree).get.asInstanceOf[SigmaStateTree]
    val afterSb = sbSubst()(afterFn).get.asInstanceOf[SigmaStateTree]
    varSubst(context)(afterSb).get.asInstanceOf[SigmaStateTree]
  }
}



//todo: write tests for PropLeaf EQ/NEQ, TxHasOutput reductions, delete this class after
object UtxoInterpreterTest extends App{
  import SchnorrSignature._

  val h1 = DLogProverInput.random()._2.h
  val h2 = DLogProverInput.random()._2.h

  val prop = TxHasOutput(GE(OutputAmount, IntLeaf(10)), EQ(OutputScript, PropLeaf(DLogNode(h2))))

  val outputToSpend = SigmaStateBox(10, prop)

  val newOutput1 = SigmaStateBox(5, DLogNode(h1))
  val newOutput2 = SigmaStateBox(10, DLogNode(h2))
  val tx = SigmaStateTransaction(Seq(), Seq(newOutput1, newOutput2))

  val context = UtxoContext(currentHeight = 100, spendingTransaction = tx, self = outputToSpend)

  println(new UtxoInterpreter().reduceToCrypto(prop, context))
}
