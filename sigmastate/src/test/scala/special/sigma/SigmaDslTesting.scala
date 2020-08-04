package special.sigma

import org.scalatest.prop.PropertyChecks
import sigmastate.interpreter.Interpreter.ScriptEnv
import org.scalacheck.{Gen, Arbitrary}
import org.scalactic.source
import org.scalatest.{PropSpec, Matchers, Tag}

import scala.util.{DynamicVariable, Success, Failure, Try}
import sigmastate.Values.SValue
import scalan.RType
import org.ergoplatform.dsl.{SigmaContractSyntax, TestContractSpec}
import sigmastate.eval.IRContext
import sigmastate.helpers.SigmaPPrint

class SigmaDslTesting extends PropSpec
    with PropertyChecks
    with Matchers
    with SigmaTestingData with SigmaContractSyntax
    with SigmaTypeGens { suite =>

  lazy val spec = TestContractSpec(suite)(new TestingIRContext)

  override def contractEnv: ScriptEnv = Map()

  implicit lazy val IR = new TestingIRContext {
    override val okPrintEvaluatedEntries: Boolean = false
    override val okMeasureOperationTime: Boolean = true
  }

  def checkEq[A,B](f: A => B)(g: A => B): A => Try[B] = { x: A =>
    val b1 = Try(f(x)); val b2 = Try(g(x))
    (b1, b2) match {
      case (res @ Success(b1), Success(b2)) =>
        assert(b1 == b2)
        res
      case (res @ Failure(t1), Failure(t2)) =>
        val c1 = rootCause(t1).getClass
        val c2 = rootCause(t2).getClass
        c1 shouldBe c2
        res
      case _ =>
        val cause = if (b1.isFailure)
          rootCause(b1.asInstanceOf[Failure[_]].exception)
        else
          rootCause(b2.asInstanceOf[Failure[_]].exception)

        sys.error(
          s"""Should succeed with the same value or fail with the same exception, but was:
            |First result: $b1
            |Second result: $b2
            |Root cause: $cause
            |""".stripMargin)
    }

  }

  def checkEq2[A,B,R](f: (A, B) => R)(g: (A, B) => R): (A,B) => Unit = { (x: A, y: B) =>
    val r1 = f(x, y); val r2 = g(x, y)
    assert(r1.getClass == r2.getClass)
    assert(r1 == r2)
  }

  def getArrayIndex(len: Int): Int = {
    val index = Gen.choose(0, len - 1)
    index.sample.get
  }

  /** Generate indices for an array of a given length.
    * @return unordered array of indices with possibly repeated elements
    */
  def genIndices(arrLength: Int): Gen[Array[Int]] = for {
    nIndexes <- Gen.choose(0, arrLength)
    indices <- Gen.containerOfN[Array, Int](nIndexes, Gen.choose(0, arrLength - 1))
  } yield indices


  case class EqualityChecker[T: RType](obj: T) {
    def apply[R: RType](dslFunc: T => R)(script: String) =
      checkEq(func[T, R](script))(dslFunc)(obj)
  }

  /** Type of the language feature to be tested. */
  sealed trait FeatureType
  case object ExistingFeature extends FeatureType
  case object AddedFeature extends FeatureType

  val LogScriptDefault: Boolean = false

  /** Test case descriptor of the language feature.
    *
    * @param featureType   type of the language feature
    * @param scalaFunc    feature semantic given by scala code
    * @param expectedExpr expression which represents the test case code
    * @param oldImpl      function that executes the feature using v3 interpreter implementation
    * @param newImpl      function that executes the feature using v4 interpreter implementation
    */
  case class FeatureTest[A, B](featureType: FeatureType,
                               script: String,
                               scalaFunc: A => B,
                               expectedExpr: Option[SValue],
                               oldImpl: () => CompiledFunc[A, B],
                               newImpl: () => CompiledFunc[A, B],
                               printExpectedExpr: Boolean = true,
                               logScript: Boolean = LogScriptDefault
                              ) {
    def printExpectedExprOff = copy(printExpectedExpr = false)

    /** Called to print test case expression (when it is not given).
      * Can be used to create regression test cases. */
    def printSuggestion(cf: CompiledFunc[_,_]): Unit = {
      print(s"No expectedExpr for ")
      SigmaPPrint.pprintln(cf.script, height = 150)
      print("Use ")
      SigmaPPrint.pprintln(cf.expr, height = 150)
      println()
    }

    /** Checks the result of the front-end compiler against expectedExpr.
      * Used to catch regression errors in front-end compiler.
      */
    def checkExpectedExprIn(cf: CompiledFunc[_,_]): Boolean = {
      expectedExpr match {
        case Some(e) =>
          if (cf.expr != e) {
            printSuggestion(cf)
            cf.expr shouldBe e
          }
        case None if printExpectedExpr =>
          printSuggestion(cf)
      }
      true
    }

    /** v3 implementation*/
    private var _oldF: CompiledFunc[A, B] = null
    def oldF: CompiledFunc[A, B] = {
      if (_oldF == null) {
        _oldF = oldImpl()
        checkExpectedExprIn(_oldF)
      }
      _oldF
    }

    /** v4 implementation*/
    private var _newF: CompiledFunc[A, B] = null
    def newF: CompiledFunc[A, B] = {
      if (_newF == null) {
        _newF = newImpl()
        checkExpectedExprIn(_newF)
      }
      _newF
    }

    /** Depending on the featureType compares the old and new implementations against
      * semantic function (scalaFunc) on the given input.
      * @param input  data which is used to execute feature
      * @return result of feature execution */
    def checkEquality(input: A, logInputOutput: Boolean = false): Try[B] = featureType match {
      case ExistingFeature =>
        // check both implementations with Scala semantic
        val oldRes = checkEq(scalaFunc)(oldF)(input)

        if (!(newImpl eq oldImpl)) {
          val newRes = checkEq(scalaFunc)(newF)(input)
          newRes shouldBe oldRes
        }
        if (logInputOutput)
          println(s"(${SigmaPPrint(input, height = 550, width = 150)}, ${SigmaPPrint(oldRes, height = 550, width = 150)}),${if (logScript) " // " + script else ""}")
        oldRes
      case AddedFeature =>
        val oldRes = Try(oldF(input))
        oldRes.isFailure shouldBe true
        if (!(newImpl eq oldImpl)) {
          val newRes = checkEq(scalaFunc)(newF)(input)
          newRes shouldBe oldRes
        }
        oldRes
    }

    /** Depending on the featureType compares the old and new implementations against
      * semantic function (scalaFunc) on the given input, also checking the given expected result.
      */
    def checkExpected(input: A, expectedResult: B): Unit = {
      featureType match {
        case ExistingFeature =>
          // check both implementations with Scala semantic
          val oldRes = checkEq(scalaFunc)(oldF)(input)
          oldRes.get shouldBe expectedResult

          if (!(newImpl eq oldImpl)) {
            val newRes = checkEq(scalaFunc)(newF)(input)
            newRes.get shouldBe expectedResult
          }

        case AddedFeature =>
          Try(oldF(input)).isFailure shouldBe true
          if (!(newImpl eq oldImpl)) {
            val newRes = checkEq(scalaFunc)(newF)(input)
            newRes.get shouldBe expectedResult
          }
      }
    }

  }

  /** Describes existing language feature which should be equally supported in both v3 and
    * v4 of the language.
    *
    * @param scalaFunc    semantic function which defines expected behavior of the given script
    * @param script       the script to be tested against semantic function
    * @param expectedExpr expected ErgoTree expression which corresponds to the given script
    * @return feature test descriptor object which can be used to execute this test case in
    *         various ways
    */
  def existingFeature[A: RType, B: RType]
      (scalaFunc: A => B, script: String, expectedExpr: SValue = null)
      (implicit IR: IRContext): FeatureTest[A, B] = {
    val oldImpl = () => func[A, B](script)
    val newImpl = oldImpl // TODO HF: use actual new implementation here
    FeatureTest(ExistingFeature, script, scalaFunc, Option(expectedExpr), oldImpl, newImpl)
  }

  /** Describes a NEW language feature which must NOT be supported in v3 and
    * must BE supported in v4 of the language.
    *
    * @param scalaFunc    semantic function which defines expected behavior of the given script
    * @param script       the script to be tested against semantic function
    * @param expectedExpr expected ErgoTree expression which corresponds to the given script
    * @return feature test descriptor object which can be used to execute this test case in
    *         various ways
    */
  def newFeature[A: RType, B: RType]
      (scalaFunc: A => B, script: String, expectedExpr: SValue = null)
      (implicit IR: IRContext): FeatureTest[A, B] = {
    val oldImpl = () => func[A, B](script)
    val newImpl = oldImpl // TODO HF: use actual new implementation here
    FeatureTest(AddedFeature, script, scalaFunc, Option(expectedExpr), oldImpl, newImpl)
  }

  val targetVersion = new DynamicVariable[Int](4)

  val versions: Seq[Int] = Array(4)

  protected override def property(testName: String, testTags: Tag*)(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
    super.property(testName, testTags:_*) {
      for (version <- versions) {
        targetVersion.withValue(version) {
          val a = testFun
        }
      }
    }
  }

  val contextGen: Gen[Context] = ergoLikeContextGen.map(c => c.toSigmaContext(IR, false))
  implicit val arbContext = Arbitrary(contextGen)
}
