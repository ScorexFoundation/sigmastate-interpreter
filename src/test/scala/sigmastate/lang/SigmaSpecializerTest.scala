package sigmastate.lang

import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.SigmaPredef._

class SigmaSpecializerTest extends PropSpec with PropertyChecks with Matchers with LangTests {

  def spec(env: Map[String, Any], x: String): SValue = {
    try {
      val parsed = SigmaParser(x).get.value
      val binder = new SigmaBinder(env)
      val bound = binder.bind(parsed)
      val st = new SigmaTree(bound)
      val typer = new SigmaTyper(env, st)
      val typed = typer.typecheck(bound)
      val spec = new SigmaSpecializer()
      spec.specialize(typed)
    } catch {
      case e: Exception =>
//        SigmaParser.logged.foreach(println)
        throw e
    }
  }

  def typefail(env: Map[String, Any], x: String, messageSubstr: String = ""): Unit = {
    try {
      val parsed = SigmaParser(x).get.value
      val binder = new SigmaBinder(env)
      val bound = binder.bind(parsed)
      val st = new SigmaTree(bound)
      val an = new SigmaTyper(env, st)
      an.tipe(bound)
      an.errors shouldBe empty
      assert(false, s"Should not typecheck: $x")
    } catch {
      case e: TyperException =>
        if (messageSubstr.nonEmpty)
          assert(e.getMessage.contains(messageSubstr)/*, s"error message '${e.getMessage}' does't contain '${messageSubstr}'"*/)
    }
  }


}
