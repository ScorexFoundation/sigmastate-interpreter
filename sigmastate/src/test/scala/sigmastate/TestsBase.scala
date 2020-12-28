package sigmastate

import org.ergoplatform.ErgoScriptPredef
import sigmastate.Values.{ErgoTree, SigmaPropValue}
import sigmastate.interpreter.Interpreter

import scala.util.DynamicVariable

trait TestsBase {

  val activatedVersions: Seq[Byte] = Array[Byte](0, 1)

  private[sigmastate] val _currActivatedVersion = new DynamicVariable[Byte](0)
  def activatedVersionInTests: Byte = _currActivatedVersion.value

  val ergoTreeVersions: Seq[Byte] =
    (0 to Interpreter.MaxSupportedScriptVersion).map(_.toByte).toArray[Byte]

  private[sigmastate] val _currErgoTreeVersion = new DynamicVariable[Byte](0)

  /** Current ErgoTree version assigned dynamically using [[CrossVersionProps]]. */
  def ergoTreeVersionInTests: Byte = _currErgoTreeVersion.value

  /** Current ErgoTree header flags assigned dynamically using [[CrossVersionProps]] and
    * ergoTreeVersionInTests.
    */
  def ergoTreeHeaderInTests: Byte = ErgoTree.headerWithVersion(ergoTreeVersionInTests)

  /** Obtains [[ErgoTree]] which corresponds to True proposition using current
    * ergoTreeHeaderInTests. */
  def TrueTree: ErgoTree = ErgoScriptPredef.TrueProp(ergoTreeHeaderInTests)

  /** Obtains [[ErgoTree]] which corresponds to False proposition using current
    * ergoTreeHeaderInTests. */
  def FalseTree: ErgoTree = ErgoScriptPredef.FalseProp(ergoTreeHeaderInTests)

  /** Transform proposition into [[ErgoTree]] using current ergoTreeHeaderInTests. */
  def mkTestErgoTree(prop: SigmaPropValue): ErgoTree =
    ErgoTree.fromProposition(ergoTreeHeaderInTests, prop)
}
