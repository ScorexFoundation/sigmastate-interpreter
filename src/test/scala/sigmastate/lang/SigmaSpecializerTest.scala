package sigmastate.lang

import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.SigmaPredef._
import sigmastate.lang.Terms.Ident

class SigmaSpecializerTest extends PropSpec with PropertyChecks with Matchers with LangTests {

  def typed(env: Map[String, SValue], x: String): SValue = {
    val parsed = SigmaParser(x).get.value
    val binder = new SigmaBinder(env)
    val bound = binder.bind(parsed)
    val typer = new SigmaTyper
    val typed = typer.typecheck(bound)
    typed
  }
  def spec(env: Map[String, SValue], typed: SValue): SValue = {
    val spec = new SigmaSpecializer()
    spec.specialize(env, typed)
  }
  def spec(code: String): SValue = {
    spec(Map(), typed(Map(), code))
  }
  def fail(env: Map[String, SValue], code: String, messageSubstr: String = ""): Unit = {
    try {
      spec(env, typed(env, code))
      assert(false, s"Should fail: $code")
    } catch {
      case e: SpecializerException =>
        if (messageSubstr.nonEmpty)
          if (!e.getMessage.contains(messageSubstr)) {
            throw new AssertionError(s"Error message '${e.getMessage}' doesn't contain '$messageSubstr'.", e)
          }
    }
  }

  property("resolve let-bound names and substitute") {
    spec(Map("X" -> IntConstant(10)),
         Ident("X", SInt)) shouldBe IntConstant(10)
    spec(Map("X" -> IntConstant(10)),
         Plus(Ident("X", SInt).asValue[SInt.type], IntConstant(1))) shouldBe Plus(10, 1)
  }

  property("substitute all let expressions in block result") {
    spec("{ let X = 10; X }") shouldBe IntConstant(10)
    spec("{ let X = 10; let Y = 20; X + Y }") shouldBe Plus(10, 20)
    spec("{ let X = 10; let Y = 20; X + Y + X }") shouldBe Plus(Plus(10, 20), 10)
    spec("{ let X = 10 + 1; X + X}") shouldBe Plus(Plus(10, 1), Plus(10, 1))
    spec("{ let X = 10; let Y = X; Y}") shouldBe IntConstant(10)
    spec("{ let X = 10; let Y = X; let Z = Y; Z }") shouldBe IntConstant(10)
    spec("{ let X = 10; let Y = X + 1; let Z = Y + X; Z + Y + X }") shouldBe
      Plus(Plus(/*Z=*/Plus(/*Y=*/Plus(10, 1), 10), /*Y=*/Plus(10, 1)), 10)
  }

  property("Option constructors") {
    fail(Map(), "None", "Option values are not supported")
    fail(Map(), "Some(10)", "Option values are not supported")
    //    bind(env, "Some(None)") shouldBe SomeValue(NoneValue(NoType))
//    bind(env, "Some(X)") shouldBe SomeValue(Ident("X"))
//    bind(env, "Some(Some(X + 1))") shouldBe
//        SomeValue(SomeValue(Plus(Ident("X").asValue[SInt.type], IntConstant(1))))
  }
}
