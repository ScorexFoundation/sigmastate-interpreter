package sigmastate.lang

import sigmastate.lang.Terms.Ident
import sigmastate.Values.{ConcreteCollection, Value, IntConstant}
import sigmastate._
import java.math.BigInteger

import sigmastate.SCollection.SByteArray
import sigmastate.interpreter.CryptoConstants

trait LangTests {

  def BoolIdent(name: String): Value[SBoolean.type] = Ident(name).asValue[SBoolean.type]
  def IntIdent(name: String): Value[SInt.type] = Ident(name).asValue[SInt.type]
  def ByteIdent(name: String): Value[SByte.type] = Ident(name).asValue[SByte.type]
  def ByteArrayIdent(name: String): Value[SByteArray] = Ident(name).asValue[SByteArray]
  def GEIdent(name: String): Value[SGroupElement.type] = Ident(name).asValue[SGroupElement.type]
  def BigIntIdent(name: String): Value[SBigInt.type] = Ident(name).asValue[SBigInt.type]


  val EV: Map[String, Any] = Map()

  val dlog = CryptoConstants.dlogGroup
  val g1 = dlog.generator
  val g2 = dlog.multiplyGroupElements(g1, g1)
  protected val n1: BigInteger = BigInt(10).underlying()
  protected val n2: BigInteger = BigInt(20).underlying()

  val env = Map(
    "x" -> 10, "y" -> 11, "c1" -> true, "c2" -> false,
    "b1" -> 1.toByte,
    "b2" -> 2.toByte,
    "arr1" -> Array[Byte](1, 2),
    "arr2" -> Array[Byte](10, 20),
    "col1" -> ConcreteCollection(IntConstant(1), IntConstant(2)),
    "col2" -> ConcreteCollection(IntConstant(10), IntConstant(20)),
    "g1" -> g1,
    "g2" -> g2,
    "n1" -> n1,
    "n2" -> n2
  )

  /** Parses string to SType tree */
  def ty(s: String): SType = SigmaParser.parseType(s).get.value
}
