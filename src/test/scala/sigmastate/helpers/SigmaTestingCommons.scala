package sigmastate.helpers

import org.ergoplatform
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, TokenId}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, Outcome, PropSpec}
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{EvaluatedValue, GroupElementConstant, TrueLeaf, Value}
import sigmastate.interpreter.CryptoConstants
import sigmastate.lang.SigmaCompiler
import sigmastate.{SBoolean, SGroupElement, SType}

import scala.language.implicitConversions

trait TestName extends PropSpec {

  implicit var testName: String = "UndefinedTestName"
  override def withFixture (test: NoArgTest): Outcome= {
    testName = s"${test.pos.get.fileName}: ${test.name}"
    super.withFixture(test)
  }
}

trait SigmaTestingCommons extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TestName {


  val fakeSelf: ErgoBox = createBox(0, TrueLeaf)

  //fake message, in a real-life a message is to be derived from a spending transaction
  val fakeMessage = Blake2b256("Hello World")

  implicit def grElemConvert(leafConstant: GroupElementConstant): CryptoConstants.EcPointType = leafConstant.value

  implicit def grLeafConvert(elem: CryptoConstants.EcPointType): Value[SGroupElement.type] = GroupElementConstant(elem)

  val compiler = new SigmaCompiler

  def compile(env: Map[String, Any], code: String): Value[SType] = {
    compiler.compile(env, code)
  }

  def createBox(value: Int,
                proposition: Value[SBoolean.type],
                additionalTokens: Seq[(TokenId, Long)] = Seq(),
                additionalRegisters: Map[NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType]] = Map())
  = ergoplatform.ErgoBox(value, proposition, additionalTokens, additionalRegisters)

}
