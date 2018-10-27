package sigmastate.helpers

import org.ergoplatform
import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import org.ergoplatform.{ErgoAddressEncoder, ErgoBox}
import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, TokenId}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{EvaluatedValue, GroupElementConstant, SValue, TrueLeaf, Value}
import sigmastate.eval.{CompiletimeCosting, Evaluation, IRContext}
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.Interpreter.{ScriptEnv, ScriptNameProp}
import sigmastate.lang.{SigmaCompiler, TransformingSigmaBuilder}
import sigmastate.{SBoolean, SGroupElement, SType}

import scala.language.implicitConversions
import scalan.{TestContexts, TestUtils}

trait SigmaTestingCommons extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers with TestUtils with TestContexts {


  val fakeSelf: ErgoBox = createBox(0, TrueLeaf)

  //fake message, in a real-life a message is to be derived from a spending transaction
  val fakeMessage = Blake2b256("Hello World")

  implicit def grElemConvert(leafConstant: GroupElementConstant): CryptoConstants.EcPointType = leafConstant.value

  implicit def grLeafConvert(elem: CryptoConstants.EcPointType): Value[SGroupElement.type] = GroupElementConstant(elem)

  val compiler = new SigmaCompiler(TransformingSigmaBuilder)

  def compile(env: ScriptEnv, code: String): Value[SType] = {
    compiler.compile(env, code, TestnetNetworkPrefix)
  }

  def createBox(value: Int,
                proposition: Value[SBoolean.type],
                additionalTokens: Seq[(TokenId, Long)] = Seq(),
                additionalRegisters: Map[NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType]] = Map())
    = ergoplatform.ErgoBox(value, proposition, additionalTokens, additionalRegisters)

  class TestingIRContext extends TestContext with IRContext with CompiletimeCosting {
    override def onCostingResult[T](env: ScriptEnv, tree: SValue, res: CostingResult[T]): Unit = {
      env.get(ScriptNameProp) match {
        case Some(name: String) =>
          emit(name, res)
        case _ =>
      }
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
          fail(s"exception check failed on $e (caused by: ${e.getCause}")
    }
  }
}
