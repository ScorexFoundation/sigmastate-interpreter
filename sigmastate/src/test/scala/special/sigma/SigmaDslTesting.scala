package special.sigma

import org.scalatest.prop.PropertyChecks
import sigmastate.interpreter.Interpreter.ScriptEnv
import org.scalacheck.Gen
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

  case class EqualityChecker[T: RType](obj: T) {
    def apply[R: RType](dslFunc: T => R)(script: String) =
      checkEq(func[T, R](script))(dslFunc)(obj)
  }

  trait ChangeType
  case object UnchangedFeature extends ChangeType
  case object AddedFeature extends ChangeType

  case class FeatureTest[A, B](
                                  changeType: ChangeType,
                                  scalaFunc: A => B,
                                  expectedExpr: Option[SValue],
                                  oldImpl: () => CompiledFunc[A, B],
                                  newImpl: () => CompiledFunc[A, B]
                              ) {

    def printSuggestion(cf: CompiledFunc[_,_]): Unit = {
      print(s"No expectedExpr for ")
      SigmaPPrint.pprintln(cf.script)
      print("Use ")
      SigmaPPrint.pprintln(cf.expr)
      println()
    }

    def checkExpectedExprIn(cf: CompiledFunc[_,_]): Boolean = {
      expectedExpr match {
        case Some(e) =>
          cf.expr shouldBe e
        case None =>
          printSuggestion(cf)
      }
      true
    }

    lazy val oldF = oldImpl().ensuring(checkExpectedExprIn(_))
    lazy val newF = newImpl().ensuring(checkExpectedExprIn(_))

    def checkEquality(input: A): Unit = changeType match {
      case UnchangedFeature =>
        // check both implementations with Scala semantic
        checkEq(scalaFunc)(oldF)(input)

        if (!(newImpl eq oldImpl)) {
          checkEq(scalaFunc)(newF)(input)
        }

      case AddedFeature =>
        Try(oldF(input)).isFailure shouldBe true
        if (!(newImpl eq oldImpl)) {
          checkEq(scalaFunc)(newF)(input)
        }
    }


    def checkExpected(input: A, expectedResult: B): Unit = {
      changeType match {
        case UnchangedFeature =>
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

  def sameFeature[A: RType, B: RType]
      (scalaFunc: A => B, script: String, expectedExpr: SValue = null)
      (implicit IR: IRContext): FeatureTest[A, B] = {
    val oldImpl = () => func[A, B](script)
    val newImpl = oldImpl // TODO HF: use actual new implementation here
    FeatureTest(UnchangedFeature, scalaFunc, Option(expectedExpr), oldImpl, newImpl)
  }

  def newFeature[A: RType, B: RType]
      (scalaFunc: A => B, script: String, expectedExpr: SValue = null)
      (implicit IR: IRContext): FeatureTest[A, B] = {
    val oldImpl = () => func[A, B](script)
    val newImpl = oldImpl // TODO HF: use actual new implementation here
    FeatureTest(AddedFeature, scalaFunc, Option(expectedExpr), oldImpl, newImpl)
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

}
