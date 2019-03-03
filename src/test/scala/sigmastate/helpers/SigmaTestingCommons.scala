package sigmastate.helpers

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, TokenId}
import org.ergoplatform.ErgoScriptPredef.TrueProp
import org.ergoplatform.{ErgoBox, ErgoLikeContext}
import org.scalacheck.Arbitrary.arbByte
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Assertion, Matchers, PropSpec}
import scalan.{Nullable, RType, TestContexts, TestUtils}
import scorex.crypto.hash.Blake2b256
import scorex.util.serialization.{VLQByteStringReader, VLQByteStringWriter}
import sigma.types.{IsPrimView, PrimViewType, View}
import sigmastate.Values.{Constant, ErgoTree, EvaluatedValue, GroupElementConstant, SValue, Value}
import sigmastate.eval.{CompiletimeCosting, Evaluation, IRContext}
import sigmastate.interpreter.Interpreter.{ScriptEnv, ScriptNameProp}
import sigmastate.interpreter.{CryptoConstants, Interpreter}
import sigmastate.lang.{SigmaCompiler, TransformingSigmaBuilder}
import sigmastate.serialization.SigmaSerializer
import sigmastate.{SGroupElement, SType}
import spire.util.Opt

import scala.annotation.tailrec
import scala.language.implicitConversions

trait SigmaTestingCommons extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers with TestUtils with TestContexts {


  val fakeSelf: ErgoBox = createBox(0, TrueProp)

  //fake message, in a real-life a message is to be derived from a spending transaction
  val fakeMessage = Blake2b256("Hello World")

  implicit def grElemConvert(leafConstant: GroupElementConstant): CryptoConstants.EcPointType = leafConstant.value

  implicit def grLeafConvert(elem: CryptoConstants.EcPointType): Value[SGroupElement.type] = GroupElementConstant(elem)

  val compiler = SigmaCompiler(TestnetNetworkPrefix, TransformingSigmaBuilder)

  def compile(env: ScriptEnv, code: String): Value[SType] = {
    compiler.compile(env, code)
  }

  def compileWithCosting(env: ScriptEnv, code: String)(implicit IR: IRContext): Value[SType] = {
    val interProp = compiler.typecheck(env, code)
    val IR.Pair(calcF, _) = IR.doCosting(env, interProp)
    val tree = IR.buildTree(calcF)
    tree
  }


  def createBox(value: Int,
                proposition: ErgoTree,
                additionalTokens: Seq[(TokenId, Long)] = Seq(),
                additionalRegisters: Map[NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType]] = Map())
  = ErgoBox(value, proposition, 0, additionalTokens, additionalRegisters)

  def createBox(value: Int,
                proposition: ErgoTree,
                creationHeight: Int)
  = ErgoBox(value, proposition, creationHeight, Seq(), Map(), ErgoBox.allZerosModifierId)

  class TestingIRContext extends TestContext with IRContext with CompiletimeCosting {
    override def onCostingResult[T](env: ScriptEnv, tree: SValue, res: RCostingResult[T]): Unit = {
      env.get(ScriptNameProp) match {
        case Some(name: String) =>
          emit(name, res)
        case _ =>
      }
    }
  }

  private def fromPrimView[A](in: A) = {
    in match {
      case IsPrimView(v) => v
      case _ => in
    }
  }

  def func[A: RType, B: RType](func: String)(implicit IR: IRContext): A => B = {
    val tA = RType[A]
    val tB = RType[B]
    val tpeA = Evaluation.rtypeToSType(tA)
    val tpeB = Evaluation.rtypeToSType(tB)
    val code =
      s"""{
         |  val func = $func
         |  val res = func(getVar[${tA.name}](1).get)
         |  res
         |}
      """.stripMargin
    val env = Interpreter.emptyEnv
    val interProp = compiler.typecheck(env, code)
    val IR.Pair(calcF, _) = IR.doCosting(env, interProp)
    val valueFun = IR.compile[tpeB.type](IR.getDataEnv, IR.asRep[IR.Context => tpeB.WrappedType](calcF))

    (in: A) => {
      implicit val cA = tA.classTag
      val x = fromPrimView(in)
      val context = ErgoLikeContext.dummy(createBox(0, TrueProp))
        .withBindings(1.toByte -> Constant[SType](x.asInstanceOf[SType#WrappedType], tpeA))
      val calcCtx = context.toSigmaContext(IR, isCost = false)
      val res = valueFun(calcCtx)
      (TransformingSigmaBuilder.unliftAny(res) match {
        case Nullable(x) => // x is a value extracted from Constant
          tB match {
            case _: PrimViewType[_, _] => // need to wrap value into PrimValue
              View.mkPrimView(x) match {
                case Opt(pv) => pv
                case _ => x // cannot wrap, so just return as is
              }
            case _ => Evaluation.toDslData(x, tpeB, isCost = false) // don't need to wrap
          }
        case _ => Evaluation.toDslData(res, tpeB, isCost = false)
      }).asInstanceOf[B]
    }
  }

  def func2[A: RType, B: RType, R: RType](func: String)(implicit IR: IRContext): (A, B) => R = {
    val tA = RType[A]
    val tB = RType[B]
    val tR = RType[R]
    val tpeA = Evaluation.rtypeToSType(tA)
    val tpeB = Evaluation.rtypeToSType(tB)
    val tpeR = Evaluation.rtypeToSType(tR)
    val code =
      s"""{
         |  val func = $func
         |  val res = func(getVar[${tA.name}](1).get, getVar[${tB.name}](2).get)
         |  res
         |}
      """.stripMargin
    val env = Interpreter.emptyEnv
    val interProp = compiler.typecheck(env, code)
    val IR.Pair(calcF, _) = IR.doCosting(env, interProp)
    val valueFun = IR.compile[tpeR.type](IR.getDataEnv, IR.asRep[IR.Context => tpeR.WrappedType](calcF))

    (in1: A, in2: B) => {
      implicit val cA = tA.classTag
      implicit val cB = tB.classTag
      val x = fromPrimView(in1)
      val y = fromPrimView(in2)
      val context = ErgoLikeContext.dummy(createBox(0, TrueProp))
        .withBindings(
          1.toByte -> Constant[SType](x.asInstanceOf[SType#WrappedType], tpeA),
          2.toByte -> Constant[SType](y.asInstanceOf[SType#WrappedType], tpeB))
      val calcCtx = context.toSigmaContext(IR, isCost = false)
      val res = valueFun(calcCtx)
      (TransformingSigmaBuilder.unliftAny(res) match {
        case Nullable(x) => // x is a value extracted from Constant
          tB match {
            case _: PrimViewType[_, _] => // need to wrap value into PrimValue
              View.mkPrimView(x) match {
                case Opt(pv) => pv
                case _ => x // cannot wrap, so just return as is
              }
            case _ => x // don't need to wrap
          }
        case _ => res
      }).asInstanceOf[R]
    }
  }

  def assertExceptionThrown(fun: => Any, assertion: Throwable => Boolean): Unit = {
    try {
      fun
      fail("exception is expected")
    }
    catch {
      case e: Throwable =>
        if (!assertion(e))
          fail(s"exception check failed on $e (root cause: ${rootCause(e)})")
    }
  }

  @tailrec
  final def rootCause(t: Throwable): Throwable =
    if (t.getCause == null) t
    else rootCause(t.getCause)

  protected def roundTripTest[T](v: T)(implicit serializer: SigmaSerializer[T, T]): Assertion = {
    // using default sigma reader/writer
    val bytes = serializer.toBytes(v)
    bytes.nonEmpty shouldBe true
    serializer.parse(SigmaSerializer.startReader(bytes)) shouldBe v

    // using ergo's(scorex) reader/writer
    val w = new VLQByteStringWriter()
    serializer.serializeWithGenericWriter(v, w)
    val byteStr = w.result()
    byteStr.nonEmpty shouldBe true
    serializer.parseWithGenericReader(new VLQByteStringReader(byteStr)) shouldEqual v
  }

  protected def roundTripTestWithPos[T](v: T)(implicit serializer: SigmaSerializer[T, T]): Assertion = {
    val randomBytesCount = Gen.chooseNum(1, 20).sample.get
    val randomBytes = Gen.listOfN(randomBytesCount, arbByte.arbitrary).sample.get.toArray
    val bytes = serializer.toBytes(v)
    serializer.parse(SigmaSerializer.startReader(bytes)) shouldBe v
    serializer.parse(SigmaSerializer.startReader(randomBytes ++ bytes, randomBytesCount)) shouldBe v
  }

}
