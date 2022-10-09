package scalan.reflection

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

object ObjA {
  class A
  object ObjB
}

class GeneratorTests extends AnyPropSpec with Matchers {
  import Generator._
  class ObjB {
    class B
  }

  def test(name: String, exp: String) = {
    val normalized = normalizeName(name)
    normalized shouldBe exp
  }

  property("scalan.reflection.Generator") {
    test("", "")
    test("A", "A")
    test("A$", "A$")
    test("A$B", "A#B")
    test("A$B$", "A#B$")
    test("scala.A$B$", "scala.A#B$")
    test("scalan.reflection.ObjA$A", "scalan.reflection.ObjA.A");
    classOf[scalan.reflection.ObjA.A]
    test("scalan.reflection.GeneratorTests$ObjB$B", "scalan.reflection.GeneratorTests#ObjB#B");
    classOf[scalan.reflection.GeneratorTests#ObjB#B]
    Class.forName("scalan.reflection.ObjA$") shouldNot be (null)
    Class.forName("scalan.reflection.ObjA$ObjB$") shouldNot be (null)
  }
}
