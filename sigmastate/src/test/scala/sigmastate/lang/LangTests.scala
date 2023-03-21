package sigmastate.lang

import org.scalatest.matchers.should.Matchers
import sigmastate.lang.Terms.{Ident, MethodCallLike}
import sigmastate.Values.{ConcreteCollection, LongConstant, SValue, SigmaBoolean, Value}
import sigmastate._

import java.math.BigInteger
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.SCollection.SByteArray
import sigmastate.basics.ProveDHTuple
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.Interpreter.ScriptEnv
import special.sigma._
import sigmastate.eval._
import sigmastate.helpers.NegativeTesting
import special.collection.Coll

trait LangTests extends Matchers with NegativeTesting {

  def BoolIdent(name: String): Value[SBoolean.type] = Ident(name).asValue[SBoolean.type]
  def IntIdent(name: String): Value[SLong.type] = Ident(name).asValue[SLong.type]
  def ByteIdent(name: String): Value[SByte.type] = Ident(name).asValue[SByte.type]
  def ByteArrayIdent(name: String): Value[SByteArray] = Ident(name).asValue[SByteArray]
  def GEIdent(name: String): Value[SGroupElement.type] = Ident(name).asValue[SGroupElement.type]
  def SigmaPropIdent(name: String): Value[SSigmaProp.type] = Ident(name).asValue[SSigmaProp.type]
  def BigIntIdent(name: String): Value[SBigInt.type] = Ident(name).asValue[SBigInt.type]

  def plus(l: SValue, r: SValue, tpe: SType = NoType): MethodCallLike =
    MethodCallLike(l, "+", IndexedSeq(r), tpe)

  val EV: ScriptEnv = Map()

  val arr1 = Array[Byte](1, 2)
  val arr2 = Array[Byte](10, 20)
  val dlog = CryptoConstants.dlogGroup
  val ecp1 = dlog.generator
  val ecp2 = dlog.multiplyGroupElements(ecp1, ecp1)
  val ecp3 = dlog.multiplyGroupElements(ecp2, ecp2)
  val ecp4 = dlog.multiplyGroupElements(ecp3, ecp3)
  val g1 = CostingSigmaDslBuilder.GroupElement(ecp1)
  val g2 = CostingSigmaDslBuilder.GroupElement(ecp2)
  val g3 = CostingSigmaDslBuilder.GroupElement(ecp3)
  val g4 = CostingSigmaDslBuilder.GroupElement(ecp4)

  protected val n1: BigInt = BigInt(10).underlying()
  protected val n2: BigInt = BigInt(20).underlying()
  protected val bigIntegerArr1: Coll[BigInt] = Colls.fromItems(n1, n2)
  protected val big: BigInteger = BigInt(Long.MaxValue).underlying().pow(2)
  protected val p1: SigmaBoolean = ProveDlog(ecp1)
  protected val p2: SigmaBoolean = ProveDlog(ecp2)
  protected val dht1: SigmaBoolean = ProveDHTuple(ecp1, ecp2, ecp3, ecp4)

  val env = Map(
    "x" -> 10, "y" -> 11, "c1" -> true, "c2" -> false,
    "height1" -> 100L, "height2" -> 200L,
    "b1" -> 1.toByte,
    "b2" -> 2.toByte,
    "arr1" -> arr1,
    "arr2" -> arr2,
    "col1" -> ConcreteCollection.fromItems(LongConstant(1), LongConstant(2)),
    "col2" -> ConcreteCollection.fromItems(LongConstant(10), LongConstant(20)),
    "g1" -> g1,
    "g2" -> g2,
    "p1" -> p1,
    "p2" -> p2,
    "n1" -> n1,
    "n2" -> n2,
    "big" -> big,
    "bigIntArr1" -> bigIntegerArr1
  )

  /** Parses string to SType tree */
  def ty(s: String): SType = SigmaParser.parseType(s)

  def assertSrcCtxForAllNodes(tree: SValue): Unit = {
    import sigmastate.kiama.rewriting.Rewriter._
    rewrite(everywherebu(rule[Any] {
      case node: SValue =>
        withClue(s"Missing sourceContext for $node") { node.sourceContext.isDefined shouldBe true }
        node
    }))(tree)
  }
}
