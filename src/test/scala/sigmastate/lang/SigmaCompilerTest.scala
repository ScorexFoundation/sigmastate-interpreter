package sigmastate.lang

import fastparse.core.ParseError
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.Terms.Ident
import sigmastate.lang.syntax.ParserException

class SigmaCompilerTest extends PropSpec with PropertyChecks with Matchers with LangTests {
  val compiler = new SigmaCompiler

  def fail(env: Map[String, Any], x: String, index: Int): Unit = {
    try {
      val res = compiler.compile(env, x)
      assert(false, s"Error expected")
    } catch {
      case e: TestFailedException =>
        throw e
      case pe: ParserException if pe.parseError.isDefined =>
        val l = pe.parseError.get.index
        l shouldBe index
    }
  }

  property("negative tests") {
    fail(env, "(10", 3)
    fail(env, "10)", 2)
    fail(env, "X)", 1)
    fail(env, "(X", 2)
    fail(env, "{ X", 3)
    fail(env, "{ let X", 7)
  }
}
