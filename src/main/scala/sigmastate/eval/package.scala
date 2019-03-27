package sigmastate

import java.math.BigInteger

import org.ergoplatform.ErgoBox
import scalan.RType
import sigmastate.Values.SigmaBoolean
import sigmastate.interpreter.CryptoConstants.EcPointType
import special.collection.{Coll, CollBuilder}
import special.sigma._

import scala.language.implicitConversions

package object eval {
  /** The primary reference to Global instance of SigmaDsl.
    * Besides operations of SigmaDslBuilder class, this instance also contains methods,
    * which are not available in Dsl code, and which are not in SigmaDslBuilder interface.
    * For example methods like `Box`, `toErgoBox` are available here, but not available in Dsl.
    * @see SigmaDslBuilder
    */
  val SigmaDsl = CostingSigmaDslBuilder

  /** The primary reference to global Coll operations. Can be used to create collections from Array etc.
    * @see CollBuilder
    */
  val Colls: CollBuilder = SigmaDsl.Colls

  /** Constructor of tuple value with more than 2 items.
    * Such long tuples are represented as Coll[Any].
    * This representaion of tuples is different from representation of pairs (x, y),
    * where Tuple2 type is used instead of Coll. */
  def TupleColl(items: Any*): Coll[Any] = Colls.fromItems(items:_*)(RType.AnyType)

  /** Implicit conversions between Dsl type and the type wrapped by the corresponding type Dsl type.
    * Here BigInt is Dsl type and BigInteger is wrapped type.
    * @see `special.sigma.CBigInt`
    */
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
