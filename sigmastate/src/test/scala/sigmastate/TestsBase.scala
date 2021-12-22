package sigmastate

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import org.ergoplatform.ErgoScriptPredef
import org.scalatest.Matchers
import sigmastate.Values.{SValue, Value, SigmaPropValue, ErgoTree, SigmaBoolean}
import sigmastate.eval.IRContext
import sigmastate.interpreter.Interpreter
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.{TransformingSigmaBuilder, SigmaCompiler, CompilerSettings}
import sigmastate.lang.Terms.ValueOps
import sigmastate.serialization.ValueSerializer
import spire.syntax.all.cfor

import scala.util.DynamicVariable

trait TestsBase extends Matchers {

  val activatedVersions: Seq[Byte] =
    (0 to Versions.MaxSupportedScriptVersion).map(_.toByte).toArray[Byte]

  private[sigmastate] val _currActivatedVersion = new DynamicVariable[Byte](0)
  def activatedVersionInTests: Byte = _currActivatedVersion.value

  /** Checks if the current activated script version used in tests corresponds to v4.x. */
  def isActivatedVersion4: Boolean = activatedVersionInTests < Versions.JitActivationVersion

  val ergoTreeVersions: Seq[Byte] =
    (0 to Versions.MaxSupportedScriptVersion).map(_.toByte).toArray[Byte]

  private[sigmastate] val _currErgoTreeVersion = new DynamicVariable[Byte](0)

  /** Current ErgoTree version assigned dynamically using [[CrossVersionProps]]. */
  def ergoTreeVersionInTests: Byte = _currErgoTreeVersion.value

  /** Current ErgoTree header flags assigned dynamically using [[CrossVersionProps]] and
    * ergoTreeVersionInTests.
    */
  def ergoTreeHeaderInTests: Byte = ErgoTree.headerWithVersion(ergoTreeVersionInTests)

  /** Executes the given block for each combination of _currActivatedVersion and
    * _currErgoTreeVersion assigned to dynamic variables.
    */
  def forEachScriptAndErgoTreeVersion
        (activatedVers: Seq[Byte], ergoTreeVers: Seq[Byte])
        (block: => Unit): Unit = {
    cfor(0)(_ < activatedVers.length, _ + 1) { i =>
      val activatedVersion = activatedVers(i)
      // setup each activated version
      _currActivatedVersion.withValue(activatedVersion) {

        cfor(0)(
          i => i < ergoTreeVers.length && ergoTreeVers(i) <= activatedVersion,
          _ + 1) { j =>
          val treeVersion = ergoTreeVers(j)
          // for each tree version up to currently activated, set it up and execute block
          _currErgoTreeVersion.withValue(treeVersion)(block)
        }

      }
    }
  }


  /** Helper method which executes the given `block` once for each `activatedVers`.
    * The method sets the dynamic variable activatedVersionInTests with is then available
    * in the block.
    */
  def forEachActivatedScriptVersion(activatedVers: Seq[Byte])(block: => Unit): Unit = {
    cfor(0)(_ < activatedVers.length, _ + 1) { i =>
      val activatedVersion = activatedVers(i)
      _currActivatedVersion.withValue(activatedVersion)(block)
    }
  }

  /** Helper method which executes the given `block` once for each `ergoTreeVers`.
    * The method sets the dynamic variable ergoTreeVersionInTests with is then available
    * in the block.
    */
  def forEachErgoTreeVersion(ergoTreeVers: Seq[Byte])(block: => Unit): Unit = {
    cfor(0)(_ < ergoTreeVers.length, _ + 1) { i =>
      val version = ergoTreeVers(i)
      _currErgoTreeVersion.withValue(version)(block)
    }
  }

  /** Obtains [[ErgoTree]] which corresponds to True proposition using current
    * ergoTreeHeaderInTests. */
  def TrueTree: ErgoTree = ErgoScriptPredef.TrueProp(ergoTreeHeaderInTests)

  /** Obtains [[ErgoTree]] which corresponds to False proposition using current
    * ergoTreeHeaderInTests. */
  def FalseTree: ErgoTree = ErgoScriptPredef.FalseProp(ergoTreeHeaderInTests)

  /** Transform proposition into [[ErgoTree]] using current ergoTreeHeaderInTests. */
  def mkTestErgoTree(prop: SigmaPropValue): ErgoTree =
    ErgoTree.fromProposition(ergoTreeHeaderInTests, prop)

  /** Transform sigma proposition into [[ErgoTree]] using current ergoTreeHeaderInTests. */
  def mkTestErgoTree(prop: SigmaBoolean): ErgoTree =
    ErgoTree.fromSigmaBoolean(ergoTreeHeaderInTests, prop)

  protected val _lowerMethodCalls = new DynamicVariable[Boolean](true)

  /** Returns true if MethodCall nodes should be lowered by TypeChecker to the
    * corresponding ErgoTree nodes. E.g. xs.map(f) -->  MapCollection(xs, f).
    * NOTE: The value of the flag is assigned dynamically using _lowerMethodCalls
    * DynamicVariable. */
  def lowerMethodCallsInTests: Boolean = _lowerMethodCalls.value

  /** If true, then all suite properties are executed with _lowerMethodCalls set to false.
    * This allow to test execution of MethodCall nodes in ErgoTree.
    */
  val okRunTestsWithoutMCLowering: Boolean = false

  val defaultCompilerSettings: CompilerSettings = CompilerSettings(
    TestnetNetworkPrefix, TransformingSigmaBuilder,
    lowerMethodCalls = true
  )

  def compilerSettingsInTests: CompilerSettings =
    defaultCompilerSettings.copy(lowerMethodCalls = lowerMethodCallsInTests)

  def compiler = SigmaCompiler(compilerSettingsInTests)

  def checkSerializationRoundTrip(v: SValue): Unit = {
    val compiledTreeBytes = ValueSerializer.serialize(v)
    withClue(s"(De)Serialization roundtrip failed for the tree:") {
      ValueSerializer.deserialize(compiledTreeBytes) shouldEqual v
    }
  }

  def compileWithoutCosting(env: ScriptEnv, code: String): Value[SType] =
    compiler.compileWithoutCosting(env, code)

  def compile(env: ScriptEnv, code: String)(implicit IR: IRContext): Value[SType] = {
    val tree = compiler.compile(env, code)
    checkSerializationRoundTrip(tree)
    tree
  }

  def compileAndCheck(env: ScriptEnv, code: String, expected: SValue)
                     (implicit IR: IRContext): (ErgoTree, SigmaPropValue) = {
    val prop = compile(env, code).asSigmaProp
    prop shouldBe expected
    val tree = mkTestErgoTree(prop)
    (tree, prop)
  }
}
