package special

import java.math.BigInteger

import org.bouncycastle.math.ec.ECPoint
import scalan.RType

import scala.reflect.{classTag, ClassTag}

package sigma {

  case class WrapperType[Wrapper](cWrapper: ClassTag[Wrapper]) extends RType[Wrapper] {
    override def classTag: ClassTag[Wrapper] = cWrapper
  }

}

package object sigma {
  def wrapperType[W: ClassTag]: RType[W] = WrapperType(classTag[W])
  
  implicit val BigIntRType: RType[BigInt] = wrapperType[BigInt]
  implicit val GroupElementRType: RType[GroupElement] = wrapperType[GroupElement]
  implicit val SigmaPropRType: RType[SigmaProp] = wrapperType[SigmaProp]
  implicit val BoxRType: RType[Box] = wrapperType[Box]
  implicit val AvlTreeRType: RType[AvlTree] = wrapperType[AvlTree]

  implicit val AnyValueRType: RType[AnyValue] = RType.fromClassTag(classTag[AnyValue])
  implicit val CostModelRType: RType[CostModel] = RType.fromClassTag(classTag[CostModel])
  implicit val ContextRType: RType[Context] = RType.fromClassTag(classTag[Context])
  implicit val SigmaContractRType: RType[SigmaContract] = RType.fromClassTag(classTag[SigmaContract])
  implicit val SigmaDslBuilderRType: RType[SigmaDslBuilder] = RType.fromClassTag(classTag[SigmaDslBuilder])

  implicit val BigIntegerRType: RType[BigInteger] = RType.fromClassTag(classTag[BigInteger])
  implicit val ECPointRType: RType[ECPoint] = RType.fromClassTag(classTag[ECPoint])
}