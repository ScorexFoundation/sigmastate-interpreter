package sigmastate.helpers

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import org.ergoplatform.ErgoBox.NonMandatoryRegisterId
import org.ergoplatform.SigmaConstants.ScriptCostLimit
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.ErgoScriptPredef.TrueProp
import org.ergoplatform.SigmaConstants.ScriptCostLimit
import org.ergoplatform._
import org.ergoplatform.validation.ValidationRules.{CheckCostFunc, CheckCalcFunc}
import org.ergoplatform.validation.{SigmaValidationSettings, ValidationRules, ValidationSpecification}
import org.scalacheck.Arbitrary.arbByte
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Assertion, Matchers, PropSpec}
import scalan.{RType, TestContexts, TestUtils}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigma.types.IsPrimView
import sigmastate.Values.{Constant, ErgoTree, EvaluatedValue, GroupElementConstant, SValue, Value}
import sigmastate.interpreter.Interpreter.{ScriptEnv, ScriptNameProp}
import sigmastate.interpreter.{ContextExtension, CryptoConstants, Interpreter}
import sigmastate.lang.{SigmaCompiler, TransformingSigmaBuilder}
import sigmastate.serialization.{GroupElementSerializer, SigmaSerializer, ValueSerializer}
import sigmastate.{AvlTreeData, SGroupElement, SType}
import sigmastate.eval.{CompiletimeCosting, Evaluation, IRContext, _}
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.utils.Helpers._
import sigmastate.helpers.TestingHelpers._
import special.collection.Coll
import special.sigma

import scala.language.implicitConversions
import scala.util.{Success, Failure, Try}

trait SigmaTestingCommons extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers with TestUtils with TestContexts with ValidationSpecification
  with NegativeTesting {

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

  case class CompiledFunc[A,B]
    (script: String, bindings: Seq[(Byte, EvaluatedValue[_ <: SType])], expr: SValue, func: A => (B, Int))
    (implicit val tA: RType[A], val tB: RType[B]) extends Function1[A, (B, Int)] {
    override def apply(x: A): (B, Int) = func(x)
  }

  /** The same operations are executed as part of Interpreter.verify() */
  def getCostingResult(env: ScriptEnv, exp: SValue)(implicit IR: IRContext): IR.RCostingResultEx[Any] = {
    val costingRes = IR.doCostingEx(env, exp, true)
    val costF = costingRes.costF
    CheckCostFunc(IR)(IR.asRep[Any => Int](costF))

    val calcF = costingRes.calcF
    CheckCalcFunc(IR)(calcF)
    costingRes
  }

  /** Returns a Scala function which is equivalent to the given function script.
    * The script is embedded into valid ErgoScript which is then compiled to
    * [[sigmastate.Values.Value]] tree.
    * Limitations:
    * 1) DeserializeContext, ConstantPlaceholder is not supported
    * @param funcScript source code of the function
    * @param bindings additional context variables
    */
  def func[A: RType, B: RType]
      (funcScript: String, bindings: (Byte, EvaluatedValue[_ <: SType])*)
      (implicit IR: IRContext): CompiledFunc[A, B] = {
    import IR._
    import IR.Context._;
    val tA = RType[A]
    val tpeA = Evaluation.rtypeToSType(tA)
    val code =
      s"""{
         |  val func = $funcScript
         |  val res = func(getVar[${tA.name}](1).get)
         |  res
         |}
      """.stripMargin
    val env = Interpreter.emptyEnv

    // The following ops are performed by frontend
    // typecheck, create graphs, compile to Tree
    // The resulting tree should be serializable
    val compiledTree = {
      val internalProp = compiler.typecheck(env, code)
      val costingRes = getCostingResult(env, internalProp)
      val calcF = costingRes.calcF
      val tree = IR.buildTree(calcF)
      checkSerializationRoundTrip(tree)
      tree
    }

    // The following is done as part of Interpreter.verify()
    val (costF, valueFun) = {
      val costingRes = getCostingResult(env, compiledTree)
      val calcF = costingRes.calcF
      val tree = IR.buildTree(calcF)

      // sanity check that buildTree is reverse to buildGraph (see doCostingEx)
      if (tA != special.sigma.ContextRType)
        tree shouldBe compiledTree

      val lA = Liftables.asLiftable[SContext, IR.Context](calcF.elem.eDom.liftable)
      val lB = Liftables.asLiftable[Any, Any](calcF.elem.eRange.liftable)
      val vf = IR.compile[SContext, Any, IR.Context, Any](IR.getDataEnv, calcF)(lA, lB)
      (costingRes.costF, vf)
    }

    val f = (in: A) => {
      implicit val cA = tA.classTag
      val x = fromPrimView(in)
      val (costingCtx, sigmaCtx) = in match {
        case ctx: CostingDataContext =>
          // the context is passed as function argument (this is for testing only)
          // This is to overcome non-functional semantics of context operations
          // (such as Inputs, Height, etc which don't have arguments and refer to the
          // context implicitly).
          // These context operations are introduced by buildTree frontend function
          // (ctx.HEIGHT method call compiled to Height IR node)
          // -------
          // We add ctx as it's own variable with id = 1
          val ctxVar = Extensions.toAnyValue[special.sigma.Context](ctx)(special.sigma.ContextRType)
          val newVars = if (ctx.vars.length < 2) {
            val vars = ctx.vars.toArray
            val buf = new Array[special.sigma.AnyValue](2)
            Array.copy(vars, 0, buf, 0, vars.length)
            buf(1) = ctxVar
            CostingSigmaDslBuilder.Colls.fromArray(buf)
          } else {
            ctx.vars.updated(1, ctxVar)
          }
          val calcCtx = ctx.copy(vars = newVars)
          val costCtx = calcCtx.copy(isCost = true)
          (costCtx, calcCtx)
        case _ =>
          val ergoCtx = ErgoLikeContextTesting.dummy(createBox(0, TrueProp))
              .withBindings(1.toByte -> Constant[SType](x.asInstanceOf[SType#WrappedType], tpeA))
              .withBindings(bindings: _*)
          val calcCtx = ergoCtx.toSigmaContext(isCost = false).asInstanceOf[CostingDataContext]
          val costCtx = calcCtx.copy(isCost = true)
          (costCtx, calcCtx)
      }

      val estimatedCost = IR.checkCostWithContext(costingCtx, costF, ScriptCostLimit.value, 0L).getOrThrow

      val (res, _) = valueFun(sigmaCtx)
      //      val (resNew, _) = ErgoTreeEvaluator.eval(ctx.asInstanceOf[ErgoLikeContext], tree)
      //      assert(resNew == res, s"The new Evaluator result differ from the old: $resNew != $res")
      (res.asInstanceOf[B], estimatedCost)
    }
    val Terms.Apply(funcVal, _) = compiledTree.asInstanceOf[SValue]
    CompiledFunc(funcScript, bindings.toSeq, funcVal, f)
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
