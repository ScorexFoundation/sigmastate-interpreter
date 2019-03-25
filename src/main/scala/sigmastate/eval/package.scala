package sigmastate

import java.math.BigInteger

import org.ergoplatform.ErgoBox
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
  implicit def bigIntToBigInteger(bi: BigInt): BigInteger = SigmaDsl.toBigInteger(bi)

  implicit def ecPointToGroupElement(p: EcPointType): GroupElement = SigmaDsl.GroupElement(p)
  implicit def groupElementToECPoint(p: GroupElement): EcPointType = SigmaDsl.toECPoint(p).asInstanceOf[EcPointType]

  implicit def sigmaBooleanToSigmaProp(p: SigmaBoolean): SigmaProp = SigmaDsl.SigmaProp(p)
  implicit def sigmaPropToSigmaBoolean(p: SigmaProp): SigmaBoolean = SigmaDsl.toSigmaBoolean(p)

  implicit def avlTreeDataToAvlTree(p: AvlTreeData): AvlTree = SigmaDsl.avlTree(p)
  implicit def avlTreeToAvlTreeData(p: AvlTree): AvlTreeData = SigmaDsl.toAvlTreeData(p)

  implicit def ergoBoxToBox(p: ErgoBox): Box = SigmaDsl.Box(p)
  implicit def boxToErgoBox(p: Box): ErgoBox = SigmaDsl.toErgoBox(p)
}
