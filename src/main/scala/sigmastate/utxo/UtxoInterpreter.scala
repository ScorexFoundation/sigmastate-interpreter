package sigmastate.utxo

import com.google.common.primitives.Bytes
import sigmastate._
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import sigmastate.interpreter.{CostAccumulator, Interpreter}

import scala.collection.mutable


class UtxoInterpreter(override val maxCost: Int = CostTable.ScriptLimit) extends Interpreter {
  override type StateT = StateTree
  override type CTX = UtxoContext

  private def replaceTxOut(idxOut: TxOutput, context: UtxoContext): AND = {
    val ts = idxOut.relation.map { rel =>
      (rel.left, rel.right) match {
        case (OutputAmount, _) | (_, OutputAmount) => OutputAmount -> rel
        case (OutputScript, _) | (_, OutputScript) => OutputScript -> rel
        case _ => ???
      }
    }

    val out = context.spendingTransaction.newBoxes(idxOut.outIndex)
    val amount = out.value
    val bindings = mutable.Map[Variable[_], Value]()

    val relations = ts.map { case (v, r) =>
      v match {
        case OutputAmount =>
          bindings.put(OutputAmount, NonNegativeIntLeaf(amount))
          r
        case OutputScript =>
          bindings.put(OutputScript, PropLeaf(out.proposition))
          r
        case _ => ???
      }
    }
    val rels = relations.map { r =>
      val rl = r.left match {
        case v: Variable[_] =>
          bindings.get(v) match {
            case Some(value) => r.withLeft(value)
            case None => r
          }
        case _ => r
      }

      rl.right match {
        case v: Variable[_] =>
          bindings.get(v) match {
            case Some(value) => rl.withRight(value)
            case None => rl
          }
        case _ => rl
      }
    }
    AND(rels)
  }

  def ssSubst(context: UtxoContext, cost: CostAccumulator): Strategy = everywherebu(rule[SigmaStateTree] {
    case Self => BoxLeaf(context.self)

    case Extract(box: BoxLeaf, field: BoxField.Field[_]) =>
      field match {
        case BoxField.Height =>
          NonNegativeIntLeaf(box.value.metadata.creationHeight)
        case BoxField.Amount =>
          NonNegativeIntLeaf(box.value.box.value)
        case BoxField.Script =>
          val leaf = PropLeaf(box.value.box.proposition)
          cost.addCost(leaf.cost).ensuring(_.isRight)
          leaf
        case BoxField.Bytes =>
          val leaf = ByteArrayLeaf(box.value.box.bytes)
          cost.addCost(leaf.cost).ensuring(_.isRight)
          leaf
        case BoxField.Register(id) =>
          val leaf = box.value.box.get(id).get
          cost.addCost(leaf.cost).ensuring(_.isRight)
          leaf
      }


    //todo: cache the bytes as a lazy val in the transaction
    case TxOutBytes =>
      val outBytes = Bytes.concat(context.spendingTransaction.newBoxes.map(_.bytes):_*)
      val leaf = ByteArrayLeaf(outBytes)
      cost.addCost(leaf.cost).ensuring(_.isRight)
      leaf

    case idxOut: TxOutput => replaceTxOut(idxOut, context)

    case hasOut: TxHasOutput =>
      val s = context.spendingTransaction.newBoxes.size
      val outConditions = (0 until s).map { idx =>
        val out = TxOutput(idx, hasOut.relation:_*)
        val and = replaceTxOut(out, context)
        cost.addCost(and.cost).ensuring(_.isRight)
        and
      }
      cost.addCost(outConditions.length).ensuring(_.isRight)
      OR(outConditions)
  })

  def varSubst(context: UtxoContext): Strategy = everywherebu(
    rule[Value] {
      case Height => NonNegativeIntLeaf(context.currentHeight)
    })

  override def specificPhases(tree: SigmaStateTree, context: UtxoContext, cost: CostAccumulator): SigmaStateTree = {
    val afterSs = ssSubst(context, cost)(tree).get.asInstanceOf[SigmaStateTree]
    varSubst(context)(afterSs).get.asInstanceOf[SigmaStateTree]
  }
}