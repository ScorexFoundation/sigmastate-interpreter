package sigmastate.lang

import org.scalatest.exceptions.TestFailedException
import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.syntax.ParserException
import sigmastate.SCollection._
import sigmastate.utxo.ByIndex

class SigmaCompilerTest extends PropSpec with PropertyChecks with Matchers with LangTests {
  val compiler = new SigmaCompiler

  def comp(env: Map[String, Any], x: String) = compiler.compile(env, x)

  def fail(env: Map[String, Any], x: String, index: Int, expected: Any): Unit = {
    try {
      val res = compiler.compile(env, x)
      assert(false, s"Error expected")
    } catch {
      case e: TestFailedException =>
        throw e
      case pe: ParserException if pe.parseError.isDefined =>
        val p = pe
        val i = pe.parseError.get.index
        val l = pe.parseError.get.lastParser
        i shouldBe index
        l.toString shouldBe expected.toString
    }
  }

  property("array indexed access") {
    comp(env, "Array(1)(0)") shouldBe
      ByIndex(ConcreteCollection(IndexedSeq(IntConstant(1)))(SInt), 0)
    comp(env, "Array(Array(1))(0)(0)") shouldBe
        ByIndex(ByIndex(ConcreteCollection(IndexedSeq(ConcreteCollection(IndexedSeq(IntConstant(1)))))(SCollection(SInt)), 0), 0)
    comp(env, "arr1(0)") shouldBe ByIndex(ByteArrayConstant(Array(1, 2)), 0)
  }

  property("array indexed access with default value") {
    comp(env, "Array(1).getOrElse(0, 1)") shouldBe
      ByIndex(ConcreteCollection(IndexedSeq(IntConstant(1)))(SInt), 0, Some(IntConstant(1)))
    comp(env, "Array(Array(1)).getOrElse(0, Array(2))(0)") shouldBe
      ByIndex(
        ByIndex(
          ConcreteCollection(IndexedSeq(ConcreteCollection(IndexedSeq(IntConstant(1)))))(SCollection(SInt)),
          0,
          Some(ConcreteCollection(Vector(IntConstant(2))))),
        0)
    comp(env, "arr1.getOrElse(999, intToByte(0))") shouldBe
      ByIndex(ByteArrayConstant(Array(1, 2)), IntConstant(999), Some(IntToByte(IntConstant(0))))
  }

  property("predefined functions") {
    comp(env, "anyOf(Array(c1, c2))") shouldBe OR(ConcreteCollection(Vector(TrueLeaf, FalseLeaf)))
    comp(env, "blake2b256(getVar[Array[Byte]](10))") shouldBe CalcBlake2b256(TaggedVariable(10, SByteArray))
    comp(env, "intToByte(10)") shouldBe IntToByte(IntConstant(10))
    comp(env, "allOf(Array(c1, c2))") shouldBe AND(ConcreteCollection(Vector(TrueLeaf, FalseLeaf)))
    comp(env, "getVar[Byte](10)") shouldBe TaggedVariable(10, SByte)
    comp(env, "getVar[Array[Byte]](10)") shouldBe TaggedVariable(10, SByteArray)
  }

  property("negative tests") {
    fail(env, "(10", 3, "\")\"")
    fail(env, "10)", 2, "End")
    fail(env, "X)", 1, "End")
    fail(env, "(X", 2, "\")\"")
    fail(env, "{ X", 3, "\"}\"")
    fail(env, "{ let X", 7, "\"=\"")
  }
}
