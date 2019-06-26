package special

import java.math.BigInteger

import org.bouncycastle.math.ec.ECPoint
import scalan.RType
import scalan.RType.GeneralType

import scala.reflect.{ClassTag, classTag}

package sigma {

  case class ArgType(override val name: String) extends RType[Any] {
    override def classTag: ClassTag[Any] = ClassTag.Any
    override def isConstantSize: Boolean = false  // pessimistic but safe default
  }
}

package object sigma {

  implicit val BigIntRType: RType[BigInt] = new GeneralType(classTag[BigInt]) {
    override def isConstantSize: Boolean = true
  }
  implicit val GroupElementRType: RType[GroupElement] = new GeneralType(classTag[GroupElement]) {
    override def isConstantSize: Boolean = true
  }
  implicit val SigmaPropRType: RType[SigmaProp] = new GeneralType(classTag[SigmaProp]) {
    override def isConstantSize: Boolean = true
  }
  implicit val AvlTreeRType:   RType[AvlTree]   = new GeneralType(classTag[AvlTree]) {
    override def isConstantSize: Boolean = true
  }

  implicit val BoxRType:       RType[Box]       = GeneralType(classTag[Box])
  implicit val ContextRType:   RType[Context]   = GeneralType(classTag[Context])

  // these are not wrapper types since they are used directly in ErgoTree values (e.g. Constants)
  // and no conversion is necessary
  implicit val HeaderRType: RType[Header] = new GeneralType(classTag[Header]) {
    override def isConstantSize: Boolean = true
  }
  implicit val PreHeaderRType: RType[PreHeader] = new GeneralType(classTag[PreHeader]) {
    override def isConstantSize: Boolean = true
  }

  implicit val AnyValueRType: RType[AnyValue] = RType.fromClassTag(classTag[AnyValue])
  implicit val CostModelRType: RType[CostModel] = RType.fromClassTag(classTag[CostModel])


  implicit val SigmaContractRType: RType[SigmaContract] = RType.fromClassTag(classTag[SigmaContract])
  implicit val SigmaDslBuilderRType: RType[SigmaDslBuilder] = RType.fromClassTag(classTag[SigmaDslBuilder])

  implicit val BigIntegerRType: RType[BigInteger] = new GeneralType(classTag[BigInteger]) {
    override def isConstantSize: Boolean = true
  }
  implicit val ECPointRType: RType[ECPoint] = new GeneralType(classTag[ECPoint]) {
    override def isConstantSize: Boolean = true
  }


  implicit val SizeAnyValueRType: RType[SizeAnyValue] = RType.fromClassTag(classTag[SizeAnyValue])
  implicit val SizeSigmaPropRType: RType[SizeSigmaProp] = RType.fromClassTag(classTag[SizeSigmaProp])
  implicit val SizeBoxRType: RType[SizeBox] = RType.fromClassTag(classTag[SizeBox])
  implicit val SizeContextRType: RType[SizeContext] = RType.fromClassTag(classTag[SizeContext])
  implicit val SizeBuilderRType: RType[SizeBuilder] = RType.fromClassTag(classTag[SizeBuilder])

  def argRType(name: String): RType[Any] = ArgType(name)
}