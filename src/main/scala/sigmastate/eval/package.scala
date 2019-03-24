package sigmastate

import java.math.BigInteger

import scalan.RType
import sigmastate.Values.SigmaBoolean
import sigmastate.interpreter.CryptoConstants.EcPointType
import special.collection.Coll
import special.sigma._

import scala.language.implicitConversions

package object eval {
  val SigmaDsl = CostingSigmaDslBuilder
  val Colls = SigmaDsl.Colls

  def TupleColl(items: Any*): Coll[Any] = Colls.fromItems(items:_*)(RType.AnyType)

  implicit def bigIntegerToBigInt(bi: BigInteger): BigInt = SigmaDsl.BigInt(bi)
  implicit def ecPointToGroupElement(p: EcPointType): GroupElement = SigmaDsl.GroupElement(p)
  implicit def groupElementToECPoint(p: GroupElement): EcPointType = SigmaDsl.toECPoint(p).asInstanceOf[EcPointType]
  implicit def sigmaBooleanToSigmaProp(p: SigmaBoolean): SigmaProp = SigmaDsl.SigmaProp(p)
}
