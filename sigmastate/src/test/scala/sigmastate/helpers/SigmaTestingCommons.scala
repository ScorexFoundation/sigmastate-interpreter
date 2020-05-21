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
import sigma.types.IsPrimView
import sigmastate.Values.{Constant, EvaluatedValue, SValue, Value, ErgoTree, GroupElementConstant}
import sigmastate.interpreter.Interpreter.{ScriptNameProp, ScriptEnv}
import sigmastate.interpreter.{EvalSettings, CryptoConstants, ErgoTreeEvaluator, Interpreter}
import sigmastate.lang.{TransformingSigmaBuilder, SigmaCompiler}
import sigmastate.serialization.{ValueSerializer, SigmaSerializer}
import sigmastate.{SGroupElement, SType}
import sigmastate.eval.{CompiletimeCosting, IRContext, Evaluation, _}
import sigmastate.interpreter.CryptoConstants.EcPointType
import special.sigma

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.{Success, Failure, Try}

trait SigmaTestingCommons extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers with TestUtils with TestContexts with ValidationSpecification {

  val fakeSelf: ErgoBox = createBox(0, TrueProp)

  val fakeContext: ErgoLikeContext = ErgoLikeContextTesting.dummy(fakeSelf)

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

  def createTransaction(dataInputs: IndexedSeq[ErgoBox],
                        outputCandidates: IndexedSeq[ErgoBoxCandidate]): ErgoLikeTransaction =
    new ErgoLikeTransaction(IndexedSeq(), dataInputs.map(b => DataInput(b.id)), outputCandidates)

  class TestingIRContext extends TestContext with IRContext with CompiletimeCosting {
    override def onCostingResult[T](env: ScriptEnv, tree: SValue, res: RCostingResultEx[T]): Unit = {
      env.get(ScriptNameProp) match {
        case Some(name: String) if saveGraphsInFile =>
          emit(name, Pair(res.calcF, res.costF))
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

    override private[sigmastate] def onResult[T](env: ScriptEnv,
                                     tree: SValue,
                                     result: RCostingResultEx[T],
                                     ctx: sigma.Context,
                                     estimatedCost: Int,
                                     calcCtx: sigma.Context,
                                     executedResult: sigma.SigmaProp,
                                     executionTime: Long): Unit = {
      if (outputComputedResults) {
        val name = env.get(Interpreter.ScriptNameProp).getOrElse("")
        println(s"ScriptName: $name, EstimatedCost: $estimatedCost, ExecutionTime: $executionTime")
      }
    }
  }

  private def fromPrimView[A](in: A) = {
    in match {
      case IsPrimView(v) => v
      case _ => in
    }
  }

  def func[A: RType, B: RType](func: String, bindings: (Byte, EvaluatedValue[_ <: SType])*)
                              (implicit IR: IRContext, context: ErgoLikeContext): A => B = {
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
    val lA = Liftables.asLiftable[SContext, IR.Context](calcF.elem.eDom.liftable)
    val lB = Liftables.asLiftable[Any, Any](calcF.elem.eRange.liftable)
    val valueFun = IR.compile[SContext, Any, IR.Context, Any](IR.getDataEnv, calcF)(lA, lB)

    (in: A) => {
      implicit val cA = tA.classTag
      val x = fromPrimView(in)
      val ctx = context
        .withBindings(1.toByte -> Constant[SType](x.asInstanceOf[SType#WrappedType], tpeA))
        .withBindings(bindings: _*)
      val calcCtx = ctx.toSigmaContext(isCost = false)
      val (res, _) = valueFun(calcCtx)
//      val (resNew, _) = ErgoTreeEvaluator.eval(ctx.asInstanceOf[ErgoLikeContext], tree)
//      assert(resNew == res, s"The new Evaluator result differ from the old: $resNew != $res")
      res.asInstanceOf[B]
    }
  }

  def funcJit[A: RType, B: RType](func: String, bindings: (Byte, EvaluatedValue[_ <: SType])*)
                              (implicit IR: IRContext, context: ErgoLikeContext, evalSettings: EvalSettings): A => B = {
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

    (in: A) => {
      implicit val cA = tA.classTag
      val x = fromPrimView(in)
      val ctx = context
        .withBindings(1.toByte -> Constant[SType](x.asInstanceOf[SType#WrappedType], tpeA))
        .withBindings(bindings: _*)
      val (res, _) = ErgoTreeEvaluator.eval(ctx.asInstanceOf[ErgoLikeContext], ErgoTree.EmptyConstants, tree, evalSettings)
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
          fail(s"exception check failed on $e (root cause: ${rootCause(e)}) \n trace:\n${e.getStackTrace.mkString("\n")}}")
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
  }

  protected def roundTripTestWithPos[T](v: T)(implicit serializer: SigmaSerializer[T, T]): Assertion = {
    val randomBytesCount = Gen.chooseNum(1, 20).sample.get
    val randomBytes = Gen.listOfN(randomBytesCount, arbByte.arbitrary).sample.get.toArray
    val bytes = serializer.toBytes(v)
    serializer.parse(SigmaSerializer.startReader(bytes)) shouldBe v
    serializer.parse(SigmaSerializer.startReader(randomBytes ++ bytes, randomBytesCount)) shouldBe v
  }

  def checkEq[A,B](f: A => B)(g: A => B): A => Unit = { x: A =>
    val b1 = Try(f(x)); val b2 = Try(g(x))
    (b1, b2) match {
      case (Success(b1), Success(b2)) =>
        assert(b1 == b2)
      case (Failure(t1), Failure(t2)) =>
        val c1 = rootCause(t1).getClass
        val c2 = rootCause(t2).getClass
        c1 shouldBe c2
      case (Failure(t1), _) => throw t1
      case (_, Failure(t2)) => throw t2
    }
  }

}
