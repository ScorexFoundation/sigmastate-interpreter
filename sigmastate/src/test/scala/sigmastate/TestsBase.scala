package sigmastate

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import org.ergoplatform.ErgoScriptPredef
import org.scalatest.Matchers
import sigmastate.Values.{SValue, Value, SigmaPropValue, ErgoTree, SigmaBoolean}
import sigmastate.eval.IRContext
import sigmastate.helpers.SigmaPPrint
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.Terms.ValueOps
import sigmastate.lang.{CompilerSettings, SigmaCompiler, TransformingSigmaBuilder}
import sigmastate.serialization.ValueSerializer

import scala.util.DynamicVariable

trait TestsBase extends Matchers with VersionTesting {

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
    val res = compiler.compile(env, code)
    val okEqual = IR.alphaEqual(res.calcF.asInstanceOf[IR.Sym], res.compiledGraph.asInstanceOf[IR.Sym])
    if (!okEqual) {
      println(s"Different graphs for $code")
    }
    if (res.calcTree != res.buildTree) {
      println(
        s"""
          |Script: $code
          |OldTree: ------------
          |${SigmaPPrint(res.calcTree, 150, 250)}
          |NewTree: ------------
          |${SigmaPPrint(res.buildTree, 150, 250)}
          |""".stripMargin)
      res.calcTree shouldBe res.buildTree
    }
    checkSerializationRoundTrip(res.calcTree)
    res.buildTree
  }

//  /** TODO v5.x: remove after AOT costing is removed */
//  def compile(env: ScriptEnv, code: String)(implicit IR: IRContext): Value[SType] = {
//    val tree = compiler.compile2(env, code)
//    checkSerializationRoundTrip(tree)
//    tree
//  }

  def compileAndCheck(env: ScriptEnv, code: String, expected: SValue)
                     (implicit IR: IRContext): (ErgoTree, SigmaPropValue) = {
    val prop = compile(env, code).asSigmaProp
    prop shouldBe expected
    val tree = mkTestErgoTree(prop)
    (tree, prop)
  }
}
