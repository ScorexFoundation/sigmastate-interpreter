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

  property("simple expressions") {
    spec(env, "x") shouldBe SInt
    spec(env, "x+y") shouldBe SInt
    spec(env, "c1 && c2") shouldBe SBoolean
    spec(env, "arr1") shouldBe SByteArray
    spec(env, "HEIGHT") shouldBe SInt
    spec(env, "HEIGHT + 1") shouldBe SInt
    spec(env, "INPUTS") shouldBe SCollection(SBox)
    spec(env, "INPUTS.size") shouldBe SInt
    spec(env, "INPUTS.size > 1") shouldBe SBoolean
    spec(env, "arr1 | arr2") shouldBe SByteArray
    spec(env, "arr1 ++ arr2") shouldBe SByteArray
    spec(env, "g1 ^ n") shouldBe SGroupElement
    spec(env, "g1 * g2") shouldBe SGroupElement
  }

  property("predefined functions") {
    spec(env, "all") shouldBe AllSym.tpe
    spec(env, "all(Array(c1, c2))") shouldBe SBoolean
  }

  property("let constructs") {
    spec(env, "{let X = 10; X > 2}") shouldBe SBoolean
    spec(env, """{let X = 10; X >= X}""".stripMargin) shouldBe SBoolean
    spec(env, """{let X = 10 + 1; X >= X}""".stripMargin) shouldBe SBoolean
    spec(env,
      """{let X = 10
       |let Y = X + 1
       |X < Y}
      """.stripMargin) shouldBe SBoolean
    spec(env, "{let X = (10, true); X._1 > 2 && X._2}") shouldBe SBoolean
    spec(env, "{let X = (Array(1,2,3), 1); X}") shouldBe STuple(SCollection(SInt), SInt)
  }

  property("generic methods of collection") {
    val minToRaise = IntConstant(1000)
    val env = this.env ++ Map(
      "minToRaise" -> minToRaise,
    )
    spec(env, "OUTPUTS.exists(fun (out: Box) = { out.value >= minToRaise })") shouldBe SBoolean
    spec(env, "OUTPUTS.map(fun (out: Box) = { out.value >= minToRaise })") shouldBe ty("Array[Boolean]")
    spec(env, "{ let arr = Array(1,2,3); arr.fold(0, fun (n1: Int, n2: Int) = n1 + n2)}") shouldBe SInt
    spec(env, "OUTPUTS.forall(fun (out: Box) = { out.value >= minToRaise })") shouldBe SBoolean
  }

  property("tuple constructor") {
    spec(env, "()") shouldBe SUnit
    spec(env, "(1)") shouldBe SInt
    spec(env, "(1, 2)") shouldBe STuple(SInt, SInt)
    spec(env, "(1, x + 1)") shouldBe STuple(SInt, SInt)
    spec(env, "(1, 2, 3)") shouldBe STuple(SInt, SInt, SInt)
    spec(env, "(1, 2 + 3, 4)") shouldBe STuple(SInt, SInt, SInt)
  }

  property("types") {
    spec(env, "{let X: Int = 10; 3 > 2}") shouldBe SBoolean
    spec(env, "{let X: (Int, Boolean) = (10, true); 3 > 2}") shouldBe SBoolean
    spec(env, "{let X: Array[Int] = Array(1,2,3); X.size}") shouldBe SInt
    spec(env, "{let X: (Array[Int], Int) = (Array(1,2,3), 1); X}") shouldBe STuple(SCollection(SInt), SInt)
    spec(env, "{let X: (Array[Int], Int) = (Array(1,2,3), x); X._1}") shouldBe SCollection(SInt)
  }

  property("if") {
    spec(env, "if(true) 1 else 2") shouldBe SInt
    spec(env, "if(c1) 1 else 2") shouldBe SInt
    spec(env, "if(c1) x else y") shouldBe SInt
    spec(env,
      """if (true) {
        |  let A = 10; A
        |} else
        |  if ( x == y) 2 else 3""".stripMargin) shouldBe SInt
  }

  property("array literals") {
    spec(env, "Array()") shouldBe SCollection(NoType)
    spec(env, "Array(Array())") shouldBe SCollection(SCollection(NoType))
    spec(env, "Array(Array(Array()))") shouldBe SCollection(SCollection(SCollection(NoType)))

    spec(env, "Array(1)") shouldBe SCollection(SInt)
    spec(env, "Array(1, x)") shouldBe SCollection(SInt)
    spec(env, "Array(Array(x + 1))") shouldBe SCollection(SCollection(SInt))

    typefail(env, "Array(1, x + 1, Array())")
    typefail(env, "Array(1, false)")
  }

  property("array indexed access") {
    typefail(env, "Array()(0)", "undefined element type")
    spec(env, "Array(0)(0)") shouldBe SInt
    typefail(env, "Array(0)(0)(0)", "function/array type is expected")
  }

  property("lambdas") {
    spec(env, "fun (a: Int) = a + 1") shouldBe SFunc(IndexedSeq(SInt), SInt)
    spec(env, "fun (a: Int): Int = a + 1") shouldBe SFunc(IndexedSeq(SInt), SInt)
    spec(env, "fun (a: Int) = { a + 1 }") shouldBe SFunc(IndexedSeq(SInt), SInt)
    spec(env, "fun (a: Int) = { let b = a + 1; b }") shouldBe SFunc(IndexedSeq(SInt), SInt)
    spec(env, "fun (a: Int, box: Box): Int = a + box.value") shouldBe
      SFunc(IndexedSeq(SInt, SBox), SInt)
    spec(env, "fun (p: (Int, GroupElement), box: Box): Int = p._1 > box.value && p._2.isIdentity") shouldBe
      SFunc(IndexedSeq(STuple(SInt, SGroupElement), SBox), SInt)

    typefail(env, "fun (a) = a + 1", "undefined type of argument")
//    typefail(env, "fun (a: Int) = Array()", "undefined type of result")
  }

  property("function definitions") {
    spec(env, "{ let f = fun (x: Int) = x + 1; f }") shouldBe SFunc(IndexedSeq(SInt), SInt)
    spec(env, "{ fun f(x: Int) = x + 1; f } ") shouldBe SFunc(IndexedSeq(SInt), SInt)
  }

  property("predefined primitives") {
    spec(env, "fun (box: Box): Int = box.value") shouldBe SFunc(IndexedSeq(SBox), SInt)
    spec(env, "fun (box: Box): ByteArray = box.propositionBytes") shouldBe SFunc(IndexedSeq(SBox), SByteArray)
    spec(env, "fun (box: Box): ByteArray = box.bytes") shouldBe SFunc(IndexedSeq(SBox), SByteArray)
    spec(env, "fun (box: Box): ByteArray = box.id") shouldBe SFunc(IndexedSeq(SBox), SByteArray)
  }

  property("compute unifying type substitution") {
    import SigmaTyper._
    def check(s1: String, s2: String, exp: Option[STypeSubst] = Some(emptySubst)): Unit = {
      val t1 = ty(s1); val t2 = ty(s2)
      unifyTypes(t1, t2) shouldBe exp
      exp match {
        case Some(subst) =>
          applySubst(t1, subst) shouldBe t2
        case None =>
      }
    }
    def unify(s1: String, s2: String, subst: (STypeIdent, SType)*): Unit =
      check(s1, s2, Some(subst.toMap))

    unifyTypes(NoType, NoType) shouldBe None
    unifyTypes(SUnit, SUnit) shouldBe Some(emptySubst)
    unifyTypes(SAny, SAny) shouldBe Some(emptySubst)
    unifyTypes(SInt, SInt) shouldBe Some(emptySubst)
    unifyTypes(SInt, SBoolean) shouldBe None

    check("(Int, Boolean)", "Int", None)
    check("(Int, Boolean)", "(Int, Boolean)")
    check("(Int, Boolean)", "(Int, Int)", None)
    check("(Int, Box)", "(Int, Box)")
    check("(Int, Box)", "(Int, Box, Boolean)", None)

    check("Array[Int]", "Array[Boolean]", None)
    check("Array[Int]", "Array[Int]")
    check("Array[(Int,Box)]", "Array[Int]", None)
    check("Array[(Int,Box)]", "Array[(Int,Box)]")
    check("Array[Array[Int]]", "Array[Array[Int]]")

    check("Int => Int", "Int => Boolean", None)
    check("Int => Int", "Int => Int")
    check("(Int, Boolean) => Int", "Int => Int", None)
    check("(Int, Boolean) => Int", "(Int,Boolean) => Int")

    unify("A", "A", ("A", STypeIdent("A")))
    check("A", "B", None)

    check("(Int, A)", "Int", None)
    unify("(Int, A)", "(Int, A)", ("A", STypeIdent("A")))
    unify("(Int, A)", "(Int, Int)", ("A", SInt))
    unify("(A, B)", "(A, B)", ("A", STypeIdent("A")), ("B", STypeIdent("B")))
    unify("(A, B)", "(Int, Boolean)", ("A", SInt), ("B", SBoolean))
    check("(A, B)", "(Int, Boolean, Box)", None)
    check("(A, Boolean)", "(Int, B)", None)
    check("(A, Int)", "(B, Int)", None)

    unify("A", "Array[Boolean]", ("A", ty("Array[Boolean]")))
    unify("Array[A]", "Array[Int]", ("A", SInt))
    unify("Array[A]", "Array[(Int, Box)]", ("A", ty("(Int, Box)")))
    unify("Array[(Int, A)]", "Array[(Int, Box)]", ("A", SBox))
    unify("Array[Array[A]]", "Array[Array[Int]]", ("A", SInt))
    unify("Array[Array[A]]", "Array[Array[A]]", ("A", STypeIdent("A")))
    check("Array[Array[A]]", "Array[Array[B]]", None)

    unify("A => Int", "Int => Int", ("A", SInt))
    check("A => Int", "Int => Boolean", None)
    unify("Int => A", "Int => Int", ("A", SInt))
    check("Int => A", "Boolean => Int", None)
    unify("(Int, A) => B", "(Int, Boolean) => Box", ("A", SBoolean), ("B", SBox))
    check("(Int, A) => A", "(Int, Boolean) => Box", None)
    unify("(Int, A) => A", "(Int, Boolean) => Boolean", ("A", SBoolean))

    unify(
      "((A,Int), Array[B] => Array[(Array[C], B)]) => A",
      "((Int,Int), Array[Boolean] => Array[(Array[C], Boolean)]) => Int",
      ("A", SInt), ("B", SBoolean), ("C", ty("C")))
  }

}
