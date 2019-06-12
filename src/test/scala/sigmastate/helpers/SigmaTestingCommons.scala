package sigmastate.helpers

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import org.ergoplatform.ErgoBox.NonMandatoryRegisterId
import org.ergoplatform.ErgoScriptPredef.TrueProp
import org.ergoplatform._
import org.ergoplatform.validation.ValidationSpecification
import org.scalacheck.Arbitrary.arbByte
import org.scalacheck.Gen
import org.scalatest.prop.{PropertyChecks, GeneratorDrivenPropertyChecks}
import org.scalatest.{PropSpec, Assertion, Matchers}
import scalan.{TestUtils, TestContexts, RType}
import scorex.crypto.hash.{Digest32, Blake2b256}
import scorex.util.serialization.{VLQByteStringReader, VLQByteStringWriter}
import sigma.types.IsPrimView
import sigmastate.Values.{Constant, EvaluatedValue, SValue, Value, ErgoTree, GroupElementConstant}
import sigmastate.interpreter.Interpreter.{ScriptNameProp, ScriptEnv}
import sigmastate.interpreter.{CryptoConstants, Interpreter}
import sigmastate.lang.{TransformingSigmaBuilder, SigmaCompiler}
import sigmastate.serialization.{ValueSerializer, SigmaSerializer}
import sigmastate.{SGroupElement, SType}
import sigmastate.eval.{CompiletimeCosting, IRContext, Evaluation, _}
import sigmastate.interpreter.CryptoConstants.EcPointType

import scala.annotation.tailrec
import scala.language.implicitConversions

trait SigmaTestingCommons extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers with TestUtils with TestContexts with ValidationSpecification {

  val fakeSelf: ErgoBox = createBox(0, TrueProp)

  val fakeContext: ErgoLikeContext = ErgoLikeContext.dummy(fakeSelf)

  //fake message, in a real-life a message is to be derived from a spending transaction
  val fakeMessage = Blake2b256("Hello World")

  implicit def grElemConvert(leafConstant: GroupElementConstant): EcPointType =
    SigmaDsl.toECPoint(leafConstant.value).asInstanceOf[EcPointType]

  implicit def grLeafConvert(elem: CryptoConstants.EcPointType): Value[SGroupElement.type] = GroupElementConstant(elem)

  val compiler = SigmaCompiler(TestnetNetworkPrefix, TransformingSigmaBuilder)

  def checkSerializationRoundTrip(v: SValue): Unit = {
    val compiledTreeBytes = ValueSerializer.serialize(v)
    withClue(s"(De)Serialization roundtrip failed for the tree:") {
      ValueSerializer.deserialize(compiledTreeBytes) shouldEqual v
    }
  }

  def compileWithoutCosting(env: ScriptEnv, code: String): Value[SType] = compiler.compileWithoutCosting(env, code)

  def compile(env: ScriptEnv, code: String)(implicit IR: IRContext): Value[SType] = {
    val tree = compiler.compile(env, code)
    checkSerializationRoundTrip(tree)
    tree
  }

  def createBox(value: Long,
                proposition: ErgoTree,
                additionalTokens: Seq[(Digest32, Long)] = Seq(),
                additionalRegisters: Map[NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType]] = Map())
  = ErgoBox(value, proposition, 0, additionalTokens, additionalRegisters)

  def createBox(value: Long,
                proposition: ErgoTree,
                creationHeight: Int)
  = ErgoBox(value, proposition, creationHeight, Seq(), Map(), ErgoBox.allZerosModifierId)

  /**
    * Create fake transaction with provided outputCandidates, but without inputs and data inputs.
    * Normally, this transaction will be invalid as far as it will break rule that sum of
    * coins in inputs should not be less then sum of coins in outputs, but we're not checking it
    * in our test cases
    */
  def createTransaction(outputCandidates: IndexedSeq[ErgoBoxCandidate]): ErgoLikeTransaction = {
    new ErgoLikeTransaction(IndexedSeq(), IndexedSeq(), outputCandidates)
  }

  def createTransaction(box: ErgoBoxCandidate): ErgoLikeTransaction = createTransaction(IndexedSeq(box))

  class TestingIRContext extends TestContext with IRContext with CompiletimeCosting {
    override def onCostingResult[T](env: ScriptEnv, tree: SValue, res: RCostingResultEx[T]): Unit = {
      env.get(ScriptNameProp) match {
        case Some(name: String) if saveGraphsInFile =>
          emit(name, res)
        case _ =>
      }
    }

    override def onEstimatedCost[T](env: ScriptEnv,
                                    tree: SValue,
                                    result: RCostingResultEx[T],
                                    ctx: special.sigma.Context,
                                    estimatedCost: Int): Unit = {
      if (outputEstimatedCost) {
        env.get(Interpreter.ScriptNameProp) match {
          case Some(name: String) =>
            println(s"Cost of $name = $estimatedCost")
          case _ =>
        }
      }
    }
  }

  private def fromPrimView[A](in: A) = {
    in match {
      case IsPrimView(v) => v
      case _ => in
    }
  }

  def func[A: RType, B: RType](func: String, bindings: (Byte, EvaluatedValue[_ <: SType])*)(implicit IR: IRContext): A => B = {
    import IR._
    import IR.Context._;
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
    val IR.Pair(calcF, _) = IR.doCosting[Any](env, interProp)
    val tree = IR.buildTree(calcF)
    checkSerializationRoundTrip(tree)
    val lA = calcF.elem.eDom.liftable.asLiftable[SContext, IR.Context]
    val lB = calcF.elem.eRange.liftable.asLiftable[Any, Any]
    val valueFun = IR.compile[SContext, Any, IR.Context, Any](IR.getDataEnv, calcF)(lA, lB)

    (in: A) => {
      implicit val cA = tA.classTag
      val x = fromPrimView(in)
      val context =
        ErgoLikeContext.dummy(createBox(0, TrueProp))
          .withBindings(1.toByte -> Constant[SType](x.asInstanceOf[SType#WrappedType], tpeA)).withBindings(bindings: _*)
      val calcCtx = context.toSigmaContext(IR, isCost = false)
      val (res, _) = valueFun(calcCtx)
      res.asInstanceOf[B]
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
    val r = SigmaSerializer.startReader(bytes)
    val positionLimitBefore = r.positionLimit
    serializer.parse(r) shouldBe v
    r.positionLimit shouldBe positionLimitBefore

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
    serializer.parse(SigmaSerializer.startReader(randomBytes ++ bytes, randomBytesCount)) shouldBe v
  }

}
