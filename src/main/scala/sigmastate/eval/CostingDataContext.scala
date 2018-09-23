package sigmastate.eval

import special.collection.{ConcreteCostedBuilder, Col, Types}
import special.sigma._

import scalan.meta.RType

class CostingBox(
    val IR: Evaluation,
    id: Col[Byte],
    value: Long,
    bytes: Col[Byte],
    bytesWithoutRef: Col[Byte],
    propositionBytes: Col[Byte],
    registers: Col[AnyValue],
    var isCost: Boolean) extends TestBox(id, value, bytes, bytesWithoutRef, propositionBytes, registers) {
  override val builder = new CostingSigmaDslBuilder(IR)

  override def getReg[T](i: Int)(implicit cT: RType[T]): Option[T] =
    if (isCost) {
      val optV = super.getReg(i)
      optV.orElse(Some(builder.Costing.defaultValue(cT)))
    } else
      super.getReg(i)
}

class CostingSigmaDslBuilder(val IR: Evaluation) extends TestSigmaDslBuilder { dsl =>
  override val Costing = new ConcreteCostedBuilder {
    import RType._
    override def defaultValue[T](valueType: RType[T]): T = (valueType match {
      case ByteType | IR.ByteElement  => 0.toByte
      case ShortType | IR.ShortElement=> 0.toShort
      case IntType | IR.IntElement  => 0
      case LongType | IR.LongElement => 0L
      case StringType | IR.StringElement => ""
      case p: PairRType[a, b] => (defaultValue(p.tA), defaultValue(p.tB))
      case col: Types.ColRType[a] => dsl.Cols.fromArray(Array[a]()(col.tA.classTag))
      case p: IR.PairElem[a, b] => (defaultValue(p.eFst), defaultValue(p.eSnd))
      case col: IR.Col.ColElem[a,_] => dsl.Cols.fromArray(Array[a]()(col.eItem.classTag))
      case _ => sys.error(s"Cannot create defaultValue($valueType)")
    }).asInstanceOf[T]
  }
}

class CostingDataContext(
    val IR: Evaluation,
    inputs: Array[Box],
    outputs: Array[Box],
    height: Long,
    selfBox: Box,
    lastBlockUtxoRootHash: AvlTree,
    vars: Array[AnyValue],
    var isCost: Boolean)
    extends TestContext(inputs, outputs, height, selfBox, lastBlockUtxoRootHash, vars)
{
  override val builder = new CostingSigmaDslBuilder(IR)

  override def getVar[T](id: Byte)(implicit cT: RType[T]) =
    if (isCost) {
      val optV = super.getVar(id)(cT)
      optV.orElse(Some(builder.Costing.defaultValue(cT)))
    } else
      super.getVar(id)(cT)
}
