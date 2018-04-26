package sigmastate

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import sigmastate.Values.{GroupElementConstant, Value}
import sigmastate.interpreter.GroupSettings
import sigmastate.lang.SigmaCompiler
import sigmastate.utxo.BoxHelpers
import sigmastate.utxo.BoxHelpers.{fakeMessage, fakeSelf}

trait SigmaTesting extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {


  implicit def grElemConvert(leafConstant: GroupElementConstant): GroupSettings.EcPointType = leafConstant.value

  implicit def grLeafConvert(elem: GroupSettings.EcPointType): Value[SGroupElement.type] = GroupElementConstant(elem)

  val compiler = new SigmaCompiler

  def compile(env: Map[String, Any], code: String): Value[SType] = {
    compiler.compile(env, code)
  }

}
