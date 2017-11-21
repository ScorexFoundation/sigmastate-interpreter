package sigmastate.utxo

import com.google.common.primitives.Bytes
import sigmastate._
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import sigmastate.interpreter.{CostAccumulator, Interpreter}


class UtxoInterpreter(override val maxCost: Int = CostTable.ScriptLimit) extends Interpreter {
  override type StateT = StateTree
  override type CTX = UtxoContext

  def ssSubst(context: UtxoContext, cost: CostAccumulator): Strategy = everywherebu(rule[SigmaStateTree] {
    case Inputs => ConcreteCollection(context.boxesToSpend.map(BoxLeafConstant.apply))

    case Outputs => ConcreteCollection(context.spendingTransaction.newBoxes
      .zipWithIndex
      .map { case (b, i) => BoxWithMetadata(b, BoxMetadata(context.currentHeight, i.toShort)) }
      .map(BoxLeafConstant.apply))

    case Self => BoxLeafConstant(context.self)


    //todo: cache the bytes as a lazy val in the transaction
    case TxOutBytes =>
      val outBytes = Bytes.concat(context.spendingTransaction.newBoxes.map(_.bytes): _*)
      val leaf = ByteArrayLeafConstant(outBytes)
      cost.addCost(leaf.cost).ensuring(_.isRight)
      leaf

    case e@Exists(coll, _) if e.evaluated =>
      e.function(coll.asInstanceOf[EvaluatedValue[CollectionLeaf[Value]]])

    case m@MapCollection(coll, mapper) if coll.evaluated =>
      m.function(coll.asInstanceOf[ConcreteCollection[Value]])

    case sum@Sum(coll) if coll.evaluated =>
      coll.asInstanceOf[ConcreteCollection[IntLeaf]].value.foldLeft(sum.zero: IntLeaf) { case (s: IntLeaf, i: IntLeaf) =>
        sum.folder(s, i): IntLeaf
      }
  })

  def functions(cost:CostAccumulator): Strategy = everywherebu(rule[Value]{
    //todo: reduce boilerplate below
    case ex@ExtractHeightInst(box: BoxLeafConstant) =>
      ex.function(box)

    case ex@ExtractAmountInst(box: BoxLeafConstant) =>
      ex.function(box)

    case ex@ExtractScriptInst(box: BoxLeafConstant) =>
      val leaf = ex.function(box)
      cost.addCost(leaf.cost).ensuring(_.isRight)
      leaf

    case ex@ExtractBytesInst(box: BoxLeafConstant) =>
      val leaf = ex.function(box)
      cost.addCost(leaf.cost).ensuring(_.isRight)
      leaf

    case ex@ExtractRegisterAsIntLeafInst(box: BoxLeafConstant, rid) =>
      val leaf = ex.function(box)
      cost.addCost(leaf.cost).ensuring(_.isRight)
      leaf
  })

  def varSubst(context: UtxoContext): Strategy = everywherebu(
    rule[Value] {
      case Height => IntLeafConstant(context.currentHeight)
    })

  override def specificPhases(tree: SigmaStateTree, context: UtxoContext, cost: CostAccumulator): SigmaStateTree = {
    val afterSs = ssSubst(context, cost)(tree).get.asInstanceOf[SigmaStateTree]
    val afterVar = varSubst(context)(afterSs).get.asInstanceOf[SigmaStateTree]
    functions(cost)(afterVar).get.asInstanceOf[SigmaStateTree]
  }
}