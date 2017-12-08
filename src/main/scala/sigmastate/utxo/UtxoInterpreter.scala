package sigmastate.utxo

import sigmastate._
import sigmastate.interpreter.Interpreter


class UtxoInterpreter(override val maxCost: Int = CostTable.ScriptLimit) extends Interpreter {
  override type StateT = StateTree
  override type CTX = UtxoContext

  override def specificTransformations(context: UtxoContext) = {
    case Inputs => ConcreteCollection(context.boxesToSpend.map(BoxLeafConstant.apply))

    case Outputs => ConcreteCollection(context.spendingTransaction.newBoxes
      .zipWithIndex
      .map { case (b, i) => BoxWithMetadata(b, BoxMetadata(context.currentHeight, i.toShort)) }
      .map(BoxLeafConstant.apply))

    case Self => BoxLeafConstant(context.self)

    case e: Exists[_] if e.transformationReady => e.input match {
      case c: ConcreteCollection[_] => e.function(c)
      case _ => ???
    }

    case f: ForAll[_] if f.transformationReady => f.input match {
      case c: ConcreteCollection[_] => f.function(c)
      case _ => ???
    }

    case m@MapCollection(coll, _, _) if m.transformationReady =>
      m.function(coll.asInstanceOf[ConcreteCollection[Value]])

    case sum@Sum(coll) if sum.transformationReady =>
      sum.function(coll.asInstanceOf[ConcreteCollection[IntLeaf]])

    case sum@SumBytes(coll, _) if sum.transformationReady =>
      sum.function(coll.asInstanceOf[ConcreteCollection[ByteArrayLeaf]])

    case Height => IntLeafConstant(context.currentHeight)

    case inst: Transformer[BoxLeaf, _]
      if inst.input.isInstanceOf[BoxLeafConstant] =>

      inst.function(inst.input.asInstanceOf[BoxLeafConstant])
  }
}