package special

import java.math.BigInteger

import org.bouncycastle.math.ec.ECPoint
import scalan.RType

import scala.reflect.{classTag, ClassTag}

package sigma {

  case class WrapperType[Wrapper](cWrapper: ClassTag[Wrapper]) extends RType[Wrapper] {
    override def classTag: ClassTag[Wrapper] = cWrapper
    override def name: String = cWrapper.runtimeClass.getSimpleName
  }

}

package object sigma {
  def wrapperType[W: ClassTag]: RType[W] = WrapperType(classTag[W])

  // TODO make these types into GeneralType (same as Header and PreHeader)
  implicit val BigIntRType: RType[BigInt] = wrapperType[BigInt]
  implicit val GroupElementRType: RType[GroupElement] = wrapperType[GroupElement]
  implicit val SigmaPropRType: RType[SigmaProp] = wrapperType[SigmaProp]
  implicit val BoxRType: RType[Box] = wrapperType[Box]
  implicit val AvlTreeRType: RType[AvlTree] = wrapperType[AvlTree]
  implicit val ContextRType: RType[Context] = wrapperType[Context]

  // these are not wrapper types since they are used directly in ErgoTree values (e.g. Constants)
  // and no conversion is necessary
  implicit val HeaderRType: RType[Header]   = RType.fromClassTag(classTag[Header])
  implicit val PreHeaderRType: RType[PreHeader] = RType.fromClassTag(classTag[PreHeader])

  implicit val AnyValueRType: RType[AnyValue] = RType.fromClassTag(classTag[AnyValue])
  implicit val CostModelRType: RType[CostModel] = RType.fromClassTag(classTag[CostModel])


  implicit val SigmaContractRType: RType[SigmaContract] = RType.fromClassTag(classTag[SigmaContract])
  implicit val SigmaDslBuilderRType: RType[SigmaDslBuilder] = RType.fromClassTag(classTag[SigmaDslBuilder])

  implicit val BigIntegerRType: RType[BigInteger] = RType.fromClassTag(classTag[BigInteger])
  implicit val ECPointRType: RType[ECPoint] = RType.fromClassTag(classTag[ECPoint])
}