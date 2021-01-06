package sigmastate

import sigmastate.interpreter.Interpreter

import scala.util.DynamicVariable

trait TestsBase {

  val activatedVersions: Seq[Byte] = Array[Byte](0, 1)

  private[sigmastate] val _currActivatedVersion = new DynamicVariable[Byte](0)
  def activatedVersionInTests: Byte = _currActivatedVersion.value

  val ergoTreeVersions: Seq[Byte] =
    (0 to Interpreter.MaxSupportedScriptVersion).map(_.toByte).toArray[Byte]

  private[sigmastate] val _currErgoTreeVersion = new DynamicVariable[Byte](0)
  def ergoTreeVersionInTests: Byte = _currErgoTreeVersion.value

}
