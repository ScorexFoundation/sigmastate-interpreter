package sigmastate.helpers

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{GroupElementConstant, TrueLeaf, Value}
import sigmastate.interpreter.GroupSettings
import sigmastate.lang.SigmaCompiler
import sigmastate.utxo.ErgoBox
import sigmastate.utxo.ErgoBox.NonMandatoryIdentifier
import sigmastate.{SBoolean, SGroupElement, SType}

trait SigmaTestingCommons extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {


  val fakeSelf: ErgoBox = createBox(0, TrueLeaf)

  //fake message, in a real-life a message is to be derived from a spending transaction
  val fakeMessage = Blake2b256("Hello World")

  implicit def grElemConvert(leafConstant: GroupElementConstant): GroupSettings.EcPointType = leafConstant.value

  implicit def grLeafConvert(elem: GroupSettings.EcPointType): Value[SGroupElement.type] = GroupElementConstant(elem)

  val compiler = new SigmaCompiler

  def compile(env: Map[String, Any], code: String): Value[SType] = {
    compiler.compile(env, code)
  }

  def createBox(value: Int,
                proposition: Value[SBoolean.type],
                additionalRegisters: Map[NonMandatoryIdentifier, _ <: Value[SType]] = Map())
  = ErgoBox(value, proposition, additionalRegisters)

}
