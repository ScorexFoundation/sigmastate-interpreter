package sigmastate.lang

import sigmastate.lang.Terms.{Ident, MethodCall}
import sigmastate.Values.{ConcreteCollection, LongConstant, SValue, SigmaBoolean, Value}
import sigmastate._
import java.math.BigInteger

import scapi.sigma.DLogProtocol.ProveDlog
import sigmastate.SCollection.SByteArray
import sigmastate.interpreter.CryptoConstants

trait LangTests {

  def BoolIdent(name: String): Value[SBoolean.type] = Ident(name).asValue[SBoolean.type]
  def IntIdent(name: String): Value[SLong.type] = Ident(name).asValue[SLong.type]
  def ByteIdent(name: String): Value[SByte.type] = Ident(name).asValue[SByte.type]
  def ByteArrayIdent(name: String): Value[SByteArray] = Ident(name).asValue[SByteArray]
  def GEIdent(name: String): Value[SGroupElement.type] = Ident(name).asValue[SGroupElement.type]
  def SigmaPropIdent(name: String): Value[SSigmaProp.type] = Ident(name).asValue[SSigmaProp.type]
  def BigIntIdent(name: String): Value[SBigInt.type] = Ident(name).asValue[SBigInt.type]

  def plus(l: SValue, r: SValue, tpe: SType = NoType): MethodCall =
    MethodCall(l, "+", IndexedSeq(r), tpe)

  val EV: Map[String, Any] = Map()

  val dlog = CryptoConstants.dlogGroup
  val g1 = dlog.generator
  val g2 = dlog.multiplyGroupElements(g1, g1)
  protected val n1: BigInteger = BigInt(10).underlying()
  protected val n2: BigInteger = BigInt(20).underlying()
  protected val big: BigInteger = BigInt(Long.MaxValue).underlying().pow(2)
  protected val p1: SigmaBoolean = ProveDlog(g1)
  protected val p2: SigmaBoolean = ProveDlog(g2)

  val env = Map(
    "x" -> 10, "y" -> 11, "c1" -> true, "c2" -> false,
    "height1" -> 100L, "height2" -> 200L,
    "b1" -> 1.toByte,
    "b2" -> 2.toByte,
    "arr1" -> Array[Byte](1, 2),
    "arr2" -> Array[Byte](10, 20),
    "col1" -> ConcreteCollection(LongConstant(1), LongConstant(2)),
    "col2" -> ConcreteCollection(LongConstant(10), LongConstant(20)),
    "g1" -> g1,
    "g2" -> g2,
    "p1" -> p1,
    "p2" -> p2,
    "n1" -> n1,
    "n2" -> n2,
    "big" -> big
  )

  /** Parses string to SType tree */
  def ty(s: String): SType = SigmaParser.parseType(s).get.value
}
