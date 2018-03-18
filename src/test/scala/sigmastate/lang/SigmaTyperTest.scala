package sigmastate.lang

import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate.lang.SigmaPredef._

class SigmaTyperTest extends PropSpec with PropertyChecks with Matchers with LangTests {

  def typecheck(env: Map[String, Any], x: String): SType = {
    try {
      val parsed = SigmaParser(x).get.value
      val binder = new SigmaBinder(env)
      val bound = binder.bind(parsed)
      val st = new SigmaTree(bound)
      val an = new SigmaTyper(env, st)
      an.errors shouldBe empty
      an.tipe(bound)
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

  property("simple expressions") {
    typecheck(env, "x") shouldBe SInt
    typecheck(env, "x+y") shouldBe SInt
    typecheck(env, "c1 && c2") shouldBe SBoolean
    typecheck(env, "arr1") shouldBe SByteArray
    typecheck(env, "HEIGHT") shouldBe SInt
    typecheck(env, "HEIGHT + 1") shouldBe SInt
    typecheck(env, "INPUTS") shouldBe SCollection(SBox)
    typecheck(env, "INPUTS.size") shouldBe SInt
    typecheck(env, "INPUTS.size > 1") shouldBe SBoolean
    typecheck(env, "arr1 | arr2") shouldBe SByteArray
    typecheck(env, "arr1 ++ arr2") shouldBe SByteArray
    typecheck(env, "g1 ^ n") shouldBe SGroupElement
    typecheck(env, "g1 * g2") shouldBe SGroupElement
  }

  property("predefined functions") {
    typecheck(env, "all") shouldBe AllSym.tpe
    typecheck(env, "all(Array(c1, c2))") shouldBe SBoolean
  }

  property("let constructs") {
    typecheck(env, "{let X = 10; X > 2}") shouldBe SBoolean
    typecheck(env, """{let X = 10; X >= X}""".stripMargin) shouldBe SBoolean
    typecheck(env, """{let X = 10 + 1; X >= X}""".stripMargin) shouldBe SBoolean
    typecheck(env,
      """{let X = 10
       |let Y = X + 1
       |X < Y}
      """.stripMargin) shouldBe SBoolean
    typecheck(env, "{let X = (10, true); X._1 > 2 && X._2}") shouldBe SBoolean
    typecheck(env, "{let X = (Array(1,2,3), 1); X}") shouldBe STuple(SCollection(SInt), SInt)
  }

//  property("predefined Exists with lambda argument") {
//    val minToRaise = IntConstant(1000)
//    val env = this.env ++ Map(
//      "minToRaise" -> minToRaise,
//    )
//    typecheck(env, """exists(OUTPUTS, fun (out: Box) = { out.amount >= minToRaise })""") shouldBe
//        Exists(Outputs, 21, GE(ExtractAmount(TaggedBox(21)), minToRaise))
//  }

  property("tuple constructor") {
    typecheck(env, "()") shouldBe SUnit
    typecheck(env, "(1)") shouldBe SInt
    typecheck(env, "(1, 2)") shouldBe STuple(SInt, SInt)
    typecheck(env, "(1, x + 1)") shouldBe STuple(SInt, SInt)
    typecheck(env, "(1, 2, 3)") shouldBe STuple(SInt, SInt, SInt)
    typecheck(env, "(1, 2 + 3, 4)") shouldBe STuple(SInt, SInt, SInt)
  }

  property("types") {
    typecheck(env, "{let X: Int = 10; 3 > 2}") shouldBe SBoolean
    typecheck(env, "{let X: (Int, Boolean) = (10, true); 3 > 2}") shouldBe SBoolean
    typecheck(env, "{let X: Array[Int] = Array(1,2,3); X.size}") shouldBe SInt
    typecheck(env, "{let X: (Array[Int], Int) = (Array(1,2,3), 1); X}") shouldBe STuple(SCollection(SInt), SInt)
    typecheck(env, "{let X: (Array[Int], Int) = (Array(1,2,3), x); X._1}") shouldBe SCollection(SInt)
  }

  property("if") {
    typecheck(env, "if(true) 1 else 2") shouldBe SInt
    typecheck(env, "if(c1) 1 else 2") shouldBe SInt
    typecheck(env, "if(c1) x else y") shouldBe SInt
    typecheck(env,
      """if (true) {
        |  let A = 10; A
        |} else
        |  if ( x == y) 2 else 3""".stripMargin) shouldBe SInt
  }

  property("array literals") {
    typecheck(env, "Array()") shouldBe SCollection(NoType)
    typecheck(env, "Array(Array())") shouldBe SCollection(SCollection(NoType))
    typecheck(env, "Array(Array(Array()))") shouldBe SCollection(SCollection(SCollection(NoType)))

    typecheck(env, "Array(1)") shouldBe SCollection(SInt)
    typecheck(env, "Array(1, x)") shouldBe SCollection(SInt)
    typecheck(env, "Array(Array(x + 1))") shouldBe SCollection(SCollection(SInt))

    typefail(env, "Array(1, x + 1, Array())")
    typefail(env, "Array(1, false)")
  }

  property("array indexed access") {
    typefail(env, "Array()(0)", "undefined element type")
    typecheck(env, "Array(0)(0)") shouldBe SInt
    typefail(env, "Array(0)(0)(0)", "function/array type is expected")
  }

  property("lambdas") {
    typecheck(env, "fun (a: Int) = a + 1") shouldBe SFunc(IndexedSeq(SInt), SInt)
    typecheck(env, "fun (a: Int): Int = a + 1") shouldBe SFunc(IndexedSeq(SInt), SInt)
    typecheck(env, "fun (a: Int) = { a + 1 }") shouldBe SFunc(IndexedSeq(SInt), SInt)
    typecheck(env, "fun (a: Int) = { let b = a + 1; b }") shouldBe SFunc(IndexedSeq(SInt), SInt)
    typecheck(env, "fun (a: Int, box: Box): Int = a + box.value") shouldBe
      SFunc(IndexedSeq(SInt, SBox), SInt)
    typecheck(env, "fun (p: (Int, GroupElement), box: Box): Int = p._1 > box.value && p._2.isIdentity") shouldBe
      SFunc(IndexedSeq(STuple(SInt, SGroupElement), SBox), SInt)

    typefail(env, "fun (a) = a + 1", "undefined type of argument")
//    typefail(env, "fun (a: Int) = Array()", "undefined type of result")
  }

  property("function definitions") {
    typecheck(env, "{ let f = fun (x: Int) = x + 1; f }") shouldBe SFunc(IndexedSeq(SInt), SInt)
    typecheck(env, "{ fun f(x: Int) = x + 1; f } ") shouldBe SFunc(IndexedSeq(SInt), SInt)
  }

  property("predefined primitives") {
    typecheck(env, "fun (box: Box): Int = box.value") shouldBe SFunc(IndexedSeq(SBox), SInt)
    typecheck(env, "fun (box: Box): ByteArray = box.propositionBytes") shouldBe SFunc(IndexedSeq(SBox), SByteArray)
    typecheck(env, "fun (box: Box): ByteArray = box.bytes") shouldBe SFunc(IndexedSeq(SBox), SByteArray)
    typecheck(env, "fun (box: Box): ByteArray = box.id") shouldBe SFunc(IndexedSeq(SBox), SByteArray)
  }
}
