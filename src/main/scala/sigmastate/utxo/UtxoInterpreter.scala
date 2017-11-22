package sigmastate.utxo

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

    case EmptyByteArray => ByteArrayLeafConstant(Array.emptyByteArray)

    case e: Exists[_] if e.transformationReady => e.input match {
      case c: ConcreteCollection[_] => e.function(c)
      case _ => ???
    }

    case f: ForAll[_] if f.transformationReady => f.input match {
      case c: ConcreteCollection[_] => f.function(c)
      case _ => ???
    }

    case m@MapCollection(coll, _) if m.transformationReady =>
      m.function(coll.asInstanceOf[ConcreteCollection[Value]])

    case sum@Sum(coll) if sum.transformationReady =>
      sum.function(coll.asInstanceOf[ConcreteCollection[IntLeaf]])

    case sum@SumBytes(coll, _) if sum.transformationReady =>
      sum.function(coll.asInstanceOf[ConcreteCollection[ByteArrayLeaf]])
  })

  def functions(cost:CostAccumulator): Strategy = everywherebu(rule[Value]{
    //todo: reduce boilerplate below
    case ex@ExtractScriptInst(box: BoxLeafConstant) =>
      val leaf = ex.function(box)
      cost.addCost(leaf.cost).ensuring(_.isRight)
      leaf

    case ex@ExtractHeightInst(box: BoxLeafConstant) =>
      ex.function(box)

    case ex@ExtractAmountInst(box: BoxLeafConstant) =>
      ex.function(box)

    case ex@ExtractBytesInst(box: BoxLeafConstant) =>
      val leaf = ex.function(box)
      cost.addCost(leaf.cost).ensuring(_.isRight)
      leaf

    case ex@ExtractRegisterAsIntLeafInst(box: BoxLeafConstant, rid) =>
      val leaf = ex.function(box)
      cost.addCost(leaf.cost).ensuring(_.isRight)
      leaf

    case ex@ExtractRegisterAsAvlTreeLeafInst(box: BoxLeafConstant, rid) =>
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