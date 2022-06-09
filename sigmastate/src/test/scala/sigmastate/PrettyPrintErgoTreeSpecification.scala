package sigmastate

import sigmastate.eval.IRContext
import sigmastate.interpreter.Interpreter
import sigmastate.lang.Terms.ValueOps
import special.sigma.SigmaDslTesting

class PrettyPrintErgoTreeSpecification extends SigmaDslTesting {
  implicit def IR: IRContext = createIR()

  private def compile(code: String, env: Interpreter.ScriptEnv = Interpreter.emptyEnv) = {
    val res = compiler.compile(env, code)
    checkCompilerResult(res)
    res.buildTree.asSigmaProp
  }

  // TODO: Add explicit braces for priority
  property("booleans"){
    val code = "false || true"
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree) shouldBe code
  }

  property("reading context of register as typed value"){
    val code = "SELF.R4[Coll[Box]].get"
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree) shouldBe code
  }

  property("function with block values in body"){
    val code =
      """{ (x: (Short, Short)) =>
        |  val a = x._1
        |  val b = x._2
        |  val plus = a + b
        |  val minus = a - b
        |  val mul = a * b
        |  val div = a / b
        |  val mod = a % b
        |  (plus, (minus, (mul, (div, mod))))
        |}""".stripMargin
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree) shouldBe
      """{ ($1: (Short, Short)) =>
        |  val $3 = $1._1
        |  val $4 = $1._2
        |  ($3 + $4, ($3 - $4, ($3 * $4, ($3 / $4, $3 % $4))))
        |}""".stripMargin
  }

  property("branching"){
    val code =
      """{ (x: Box) =>
        |  val tagOpt = x.R5[Short]
        |  if (tagOpt.isDefined) {
        |    tagOpt.get
        |  } else {
        |    0.toShort
        |  }
        |}""".stripMargin
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree, 4) shouldBe
      """{ ($1: Box) =>
        |    val $3 = $1.R5[Short]
        |    if ($3.isDefined) {
        |        $3.get
        |    } else {
        |        0.toShort
        |    }
        |}""".stripMargin
  }

  property("group generator"){
    val code = "{ (x: Int) => groupGenerator }"
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree) shouldBe
      """{ ($1: Int) =>
        |  Global.groupGenerator
        |}""".stripMargin
  }

  property("OUTPUTS"){
    val code = "OUTPUTS.exists({ (box: Box) => box.value + 5 > 10 })"
    val compiledTree = compile(code)
    PrettyPrintErgoTree.prettyPrint(compiledTree) shouldBe
      """OUTPUTS.exists({ ($1: Box) =>
        |  $1.value + 5.toLong > 10.toLong
        |})""".stripMargin
  }
}