package special

import java.math.BigInteger

import org.bouncycastle.math.ec.ECPoint
import scalan.RType

import scala.reflect.classTag

package sigma {

}

package object sigma {
  implicit val BigIntRType: RType[BigInt] = RType.fromClassTag(classTag[BigInt])
  implicit val GroupElementRType: RType[GroupElement] = RType.fromClassTag(classTag[GroupElement])
  implicit val SigmaPropRType: RType[SigmaProp] = RType.fromClassTag(classTag[SigmaProp])
  implicit val BoxRType: RType[Box] = RType.fromClassTag(classTag[Box])
  implicit val AnyValueRType: RType[AnyValue] = RType.fromClassTag(classTag[AnyValue])
  implicit val CostModelRType: RType[CostModel] = RType.fromClassTag(classTag[CostModel])
  implicit val AvlTreeRType: RType[AvlTree] = RType.fromClassTag(classTag[AvlTree])
  implicit val ContextRType: RType[Context] = RType.fromClassTag(classTag[Context])
  implicit val SigmaContractRType: RType[SigmaContract] = RType.fromClassTag(classTag[SigmaContract])
  implicit val SigmaDslBuilderRType: RType[SigmaDslBuilder] = RType.fromClassTag(classTag[SigmaDslBuilder])
  implicit val BigIntegerRType: RType[BigInteger] = RType.fromClassTag(classTag[BigInteger])
  implicit val ECPointRType: RType[ECPoint] = RType.fromClassTag(classTag[ECPoint])
}