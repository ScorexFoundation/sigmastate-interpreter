package sigmastate.helpers

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import org.ergoplatform.{ErgoLikeContext, ErgoAddressEncoder, ErgoBox}
import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, TokenId}
import org.scalatest.prop.{PropertyChecks, GeneratorDrivenPropertyChecks}
import org.scalatest.{PropSpec, Matchers}
import scorex.crypto.hash.Blake2b256
import scorex.util._
import sigmastate.Values.{Constant, EvaluatedValue, SValue, TrueLeaf, Value, GroupElementConstant}
import sigmastate.eval.{CompiletimeCosting, IRContext, Evaluation}
import sigmastate.interpreter.{CryptoConstants, Interpreter}
import sigmastate.interpreter.Interpreter.{ScriptNameProp, ScriptEnv}
import sigmastate.lang.{TransformingSigmaBuilder, SigmaCompiler}
import sigmastate.{SGroupElement, SBoolean, SType}

import scala.annotation.tailrec
import scala.language.implicitConversions
import scalan.{TestUtils, TestContexts, Nullable, RType}
import sigma.types.{View, IsPrimView, PrimViewType}
import spire.util.Opt

trait SigmaTestingCommons extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers with TestUtils with TestContexts {


  val fakeSelf: ErgoBox = createBox(0, TrueLeaf)

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
                proposition: Value[SBoolean.type],
                additionalTokens: Seq[(TokenId, Long)] = Seq(),
                additionalRegisters: Map[NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType]] = Map())
    = ErgoBox(value, proposition, 0, additionalTokens, additionalRegisters)

  def createBox(value: Int,
                proposition: Value[SBoolean.type],
                creationHeight: Int)
    = ErgoBox(value, proposition, creationHeight, Seq(), Map(), ErgoBox.allZerosModifierId)

  class TestingIRContext extends TestContext with IRContext with CompiletimeCosting {
    override def onCostingResult[T](env: ScriptEnv, tree: SValue, res: CostingResult[T]): Unit = {
      env.get(ScriptNameProp) match {
        case Some(name: String) =>
          emit(name, res)
        case _ =>
      }
    }
  }

  def func[A:RType,B:RType](func: String)(implicit IR: IRContext): A => B = {
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
      val x = in match {
        case IsPrimView(v) => v
        case _ => in
      }
      val context = ErgoLikeContext.dummy(createBox(0, TrueLeaf))
          .withBindings(1.toByte -> Constant[SType](x.asInstanceOf[SType#WrappedType], tpeA))
      val calcCtx = context.toSigmaContext(IR, isCost = false)
      val res = valueFun(calcCtx)
      (TransformingSigmaBuilder.unliftAny(res) match {
        case Nullable(x) => // x is a value extracted from Constant
          tB match {
            case _: PrimViewType[_,_] => // need to wrap value into PrimValue
              View.mkPrimView(x) match {
                case Opt(pv) => pv
                case _ => x  // cannot wrap, so just return as is
              }
            case _ => x  // don't need to wrap
          }
        case _ => res
      }).asInstanceOf[B]
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
}
