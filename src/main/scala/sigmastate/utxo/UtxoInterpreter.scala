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
    val bindings = mutable.Map[NotReadyValue[_], Value]()

    val relations = ts.map { case (v, r) =>
      v match {
        case OutputAmount =>
          bindings.put(OutputAmount, IntLeafConstant(amount))
          r
        case OutputScript =>
          bindings.put(OutputScript, PropLeafConstant(out.proposition))
          r
        case _ => ???
      }
    }
    val rels = relations.map { r =>
      val rl = r.left match {
        case v: NotReadyValue[_] =>
          bindings.get(v) match {
            case Some(value) => r.withLeft(value)
            case None => r
          }
        case _ => r
      }

      rl.right match {
        case v: NotReadyValue[_] =>
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
    case Inputs => ConcreteCollection(context.boxesToSpend.map(BoxLeafConstant.apply))

    case Outputs => ConcreteCollection(context.spendingTransaction.newBoxes
      .zipWithIndex
      .map { case (b, i) => BoxWithMetadata(b, BoxMetadata(context.currentHeight, i.toShort)) }
      .map(BoxLeafConstant.apply))

    case Self => BoxLeafConstant(context.self)

      //todo: reduce boilerplate below
    case ex@ExtractHeightInst(box: BoxLeafConstant) => ex.function(box)

    case ex@ExtractAmountInst(box: BoxLeafConstant) => ex.function(box)

    case ex@ExtractScriptInst(box: BoxLeafConstant) =>
      val leaf = ex.function(box)
      cost.addCost(leaf.cost).ensuring(_.isRight)
      leaf

    case ex@ExtractBytesInst(box: BoxLeafConstant) =>
      val leaf = ex.function(box)
      cost.addCost(leaf.cost).ensuring(_.isRight)
      leaf

    case ex@ExtractRegisterAsIntLeaf(box: BoxLeafConstant, rid) =>
      val leaf = ex.function(box)
      cost.addCost(leaf.cost).ensuring(_.isRight)
      leaf

    //todo: cache the bytes as a lazy val in the transaction
    case TxOutBytes =>
      val outBytes = Bytes.concat(context.spendingTransaction.newBoxes.map(_.bytes): _*)
      val leaf = ByteArrayLeafConstant(outBytes)
      cost.addCost(leaf.cost).ensuring(_.isRight)
      leaf

    case idxOut: TxOutput => replaceTxOut(idxOut, context)

    case hasOut: TxHasOutput =>
      val s = context.spendingTransaction.newBoxes.size
      val outConditions = (0 until s).map { idx =>
        val out = TxOutput(idx, hasOut.relation: _*)
        val and = replaceTxOut(out, context)
        cost.addCost(and.cost).ensuring(_.isRight)
        and
      }
      cost.addCost(outConditions.length).ensuring(_.isRight)
      OR(outConditions)
  })

  def varSubst(context: UtxoContext): Strategy = everywherebu(
    rule[Value] {
      case Height => IntLeafConstant(context.currentHeight)
    })

  override def specificPhases(tree: SigmaStateTree, context: UtxoContext, cost: CostAccumulator): SigmaStateTree = {
    val afterSs = ssSubst(context, cost)(tree).get.asInstanceOf[SigmaStateTree]
    varSubst(context)(afterSs).get.asInstanceOf[SigmaStateTree]
  }
}