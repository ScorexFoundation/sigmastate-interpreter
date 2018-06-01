package sigmastate.lang

import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import sigmastate._
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate.lang.SigmaPredef._

class SigmaTyperTest extends PropSpec with PropertyChecks with Matchers with LangTests {

  def typecheck(env: Map[String, Any], x: String): SType = {
    try {
      val parsed = SigmaParser(x).get.value
      val binder = new SigmaBinder(env)
      val bound = binder.bind(parsed)
      val st = new SigmaTree(bound)
      val typer = new SigmaTyper
      val typed = typer.typecheck(bound)
     typed.tpe
    } catch {
      case e: Exception => throw e
    }
  }

  def typefail(env: Map[String, Any], x: String, messageSubstr: String = ""): Unit = {
    try {
      val parsed = SigmaParser(x).get.value
      val binder = new SigmaBinder(env)
      val bound = binder.bind(parsed)
      val st = new SigmaTree(bound)
      val typer = new SigmaTyper
      val typed = typer.typecheck(bound)
      assert(false, s"Should not typecheck: $x")
    } catch {
      case e: TyperException =>
        if (messageSubstr.nonEmpty)
          if (!e.getMessage.contains(messageSubstr)) {
            throw new AssertionError(s"Error message '${e.getMessage}' doesn't contain '$messageSubstr'.", e)
          }
    }
  }

  property("simple expressions") {
    typecheck(env, "x") shouldBe SLong
    typecheck(env, "x + y") shouldBe SLong
    typecheck(env, "x - y") shouldBe SLong
    typecheck(env, "x / y") shouldBe SLong
    typecheck(env, "x % y") shouldBe SLong
    typecheck(env, "c1 && c2") shouldBe SBoolean
    typecheck(env, "arr1") shouldBe SByteArray
    typecheck(env, "HEIGHT") shouldBe SLong
    typecheck(env, "HEIGHT + 1") shouldBe SLong
    typecheck(env, "INPUTS") shouldBe SCollection(SBox)
    typecheck(env, "INPUTS.size") shouldBe SLong
    typecheck(env, "INPUTS.size > 1") shouldBe SBoolean
    typecheck(env, "arr1 | arr2") shouldBe SByteArray
    typecheck(env, "arr1 ++ arr2") shouldBe SByteArray
    typecheck(env, "col1 ++ col2") shouldBe SCollection(SLong)
    typecheck(env, "g1 ^ n1") shouldBe SGroupElement
    typecheck(env, "g1 * g2") shouldBe SGroupElement
    typecheck(env, "b1 < b2") shouldBe SBoolean
    typecheck(env, "b1 > b2") shouldBe SBoolean
    typecheck(env, "b1 <= b2") shouldBe SBoolean
    typecheck(env, "b1 >= b2") shouldBe SBoolean
    typecheck(env, "n1 < n2") shouldBe SBoolean
    typecheck(env, "n1 > n2") shouldBe SBoolean
    typecheck(env, "n1 <= n2") shouldBe SBoolean
    typecheck(env, "n1 >= n2") shouldBe SBoolean
  }

  property("predefined functions") {
    typecheck(env, "allOf") shouldBe AllSym.tpe
    typecheck(env, "allOf(Array(c1, c2))") shouldBe SBoolean
    typecheck(env, "getVar[Byte](10)") shouldBe SByte
    typecheck(env, "getVar[Array[Byte]](10)") shouldBe SByteArray
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
    typecheck(env, "{let X = (Array(1,2,3), 1); X}") shouldBe STuple(SCollection(SLong), SLong)
  }

  property("generic methods of arrays") {
    val minToRaise = LongConstant(1000)
    val env = this.env ++ Map(
      "minToRaise" -> minToRaise
    )
    typecheck(env, "OUTPUTS.map(fun (out: Box) = { out.value >= minToRaise })") shouldBe ty("Array[Boolean]")
    typecheck(env, "OUTPUTS.exists(fun (out: Box) = { out.value >= minToRaise })") shouldBe SBoolean
    typecheck(env, "OUTPUTS.forall(fun (out: Box) = { out.value >= minToRaise })") shouldBe SBoolean
    typecheck(env, "{ let arr = Array(1,2,3); arr.fold(0, fun (i1: Int, i2: Int) = i1 + i2)}") shouldBe SLong
    typecheck(env, "OUTPUTS.slice(0, 10)") shouldBe ty("Array[Box]")
    typecheck(env, "OUTPUTS.where(fun (out: Box) = { out.value >= minToRaise })") shouldBe ty("Array[Box]")
  }

  property("tuple constructor") {
    typecheck(env, "()") shouldBe SUnit
    typecheck(env, "(1)") shouldBe SLong
    typecheck(env, "(1, 2)") shouldBe STuple(SLong, SLong)
    typecheck(env, "(1, x + 1)") shouldBe STuple(SLong, SLong)
    typecheck(env, "(1, 2, 3)") shouldBe STuple(SLong, SLong, SLong)
    typecheck(env, "(1, 2 + 3, 4)") shouldBe STuple(SLong, SLong, SLong)
  }

  property("types") {
    typecheck(env, "{let X: Int = 10; 3 > 2}") shouldBe SBoolean
    typecheck(env, "{let X: (Int, Boolean) = (10, true); 3 > 2}") shouldBe SBoolean
    typecheck(env, "{let X: Array[Int] = Array(1,2,3); X.size}") shouldBe SLong
    typecheck(env, "{let X: (Array[Int], Int) = (Array(1,2,3), 1); X}") shouldBe STuple(SCollection(SLong), SLong)
    typecheck(env, "{let X: (Array[Int], Int) = (Array(1,2,3), x); X._1}") shouldBe SCollection(SLong)
  }

  property("if") {
    typecheck(env, "if(true) 1 else 2") shouldBe SLong
    typecheck(env, "if(c1) 1 else 2") shouldBe SLong
    typecheck(env, "if(c1) x else y") shouldBe SLong
    typecheck(env,
      """if (true) {
        |  let A = 10; A
        |} else
        |  if ( x == y) 2 else 3""".stripMargin) shouldBe SLong
  }

  property("array literals") {
    typefail(env, "Array()", "Undefined type of empty collection")
    typefail(env, "Array(Array())", "Undefined type of empty collection")
    typefail(env, "Array(Array(Array()))", "Undefined type of empty collection")

    typecheck(env, "Array(1)") shouldBe SCollection(SLong)
    typecheck(env, "Array(1, x)") shouldBe SCollection(SLong)
    typecheck(env, "Array(Array(x + 1))") shouldBe SCollection(SCollection(SLong))

    typefail(env, "Array(1, x + 1, Array())")
    typefail(env, "Array(1, false)")
  }

  property("Option constructors") {
    typecheck(env, "Some(10)") shouldBe SOption(SLong)
    typecheck(env, "Some(x)") shouldBe SOption(SLong)
    typecheck(env, "Some(x + 1)") shouldBe SOption(SLong)
    typecheck(env, "Some(Some(10))") shouldBe SOption(SOption(SLong))
  }

  property("array indexed access") {
    typefail(env, "Array()(0)", "Undefined type of empty collection")
    typecheck(env, "Array(0)(0)") shouldBe SLong
    typefail(env, "Array(0)(0)(0)", "array type is expected")
  }

  property("array indexed access with evaluation") {
    typecheck(env, "Array(0)(1 - 1)") shouldBe SLong
    typecheck(env, "Array(0)((1 - 1) + 0)") shouldBe SLong
    typefail(env, "Array(0)(0 == 0)", "Invalid argument type")
    typefail(env, "Array(0)(1,1,1)", "Invalid argument of array application")
  }

  property("lambdas") {
    typecheck(env, "fun (a: Int) = a + 1") shouldBe SFunc(IndexedSeq(SLong), SLong)
    typecheck(env, "fun (a: Int): Int = a + 1") shouldBe SFunc(IndexedSeq(SLong), SLong)
    typecheck(env, "fun (a: Int) = { a + 1 }") shouldBe SFunc(IndexedSeq(SLong), SLong)
    typecheck(env, "fun (a: Int) = { let b = a + 1; b }") shouldBe SFunc(IndexedSeq(SLong), SLong)
    typecheck(env, "fun (a: Int, box: Box): Int = a + box.value") shouldBe
      SFunc(IndexedSeq(SLong, SBox), SLong)
    typecheck(env, "fun (p: (Int, GroupElement), box: Box): Boolean = p._1 > box.value && p._2.isIdentity") shouldBe
      SFunc(IndexedSeq(STuple(SLong, SGroupElement), SBox), SBoolean)

    typefail(env, "fun (a) = a + 1", "undefined type of argument")
  }

  property("function definitions") {
    typecheck(env, "{ let f = fun (x: Int) = x + 1; f }") shouldBe SFunc(IndexedSeq(SLong), SLong)
    typecheck(env, "{ fun f(x: Int) = x + 1; f } ") shouldBe SFunc(IndexedSeq(SLong), SLong)
  }

  property("predefined primitives") {
    typecheck(env, "fun (box: Box): Int = box.value") shouldBe SFunc(IndexedSeq(SBox), SLong)
    typecheck(env, "fun (box: Box): Array[Byte] = box.propositionBytes") shouldBe SFunc(IndexedSeq(SBox), SByteArray)
    typecheck(env, "fun (box: Box): Array[Byte] = box.bytes") shouldBe SFunc(IndexedSeq(SBox), SByteArray)
    typecheck(env, "fun (box: Box): Array[Byte] = box.id") shouldBe SFunc(IndexedSeq(SBox), SByteArray)
  }

  property("type parameters") {
    typecheck(env, "SELF.R1[Int]") shouldBe SOption(SLong)
    typecheck(env, "SELF.R1[Int].isDefined") shouldBe SBoolean
    typecheck(env, "SELF.R1[Int].value") shouldBe SLong
    typefail(env, "X[Int]", "expression doesn't have type parameters")
    typefail(env, "arr1[Int]", "expression doesn't have type parameters")
    typecheck(env, "SELF.R1[(Int,Boolean)]") shouldBe SOption(STuple(SLong, SBoolean))
    typecheck(env, "SELF.R1[(Int,Boolean)].value") shouldBe STuple(SLong, SBoolean)
    typefail(env, "SELF.R1[Int,Boolean].value", "Wrong number of type arguments")
    typecheck(env, "Array[Int]()") shouldBe SCollection(SLong)
  }
  
  property("compute unifying type substitution") {
    import SigmaTyper._
    def checkTypes(t1: SType, t2: SType, exp: Option[STypeSubst]): Unit = {
      unifyTypes(t1, t2) shouldBe exp
      exp match {
        case Some(subst) =>
          unifyTypes(applySubst(t1, subst), t2) shouldBe Some(emptySubst)
        case None =>
      }
    }
    def check(s1: String, s2: String, exp: Option[STypeSubst] = Some(emptySubst)): Unit = {
      val t1 = ty(s1); val t2 = ty(s2)
      checkTypes(t1, t2, exp)
    }
    def unify(s1: String, s2: String, subst: (STypeIdent, SType)*): Unit =
      check(s1, s2, Some(subst.toMap))

    unifyTypes(NoType, NoType) shouldBe None
    unifyTypes(SUnit, SUnit) shouldBe Some(emptySubst)
    unifyTypes(SAny, SAny) shouldBe Some(emptySubst)
    unifyTypes(SLong, SLong) shouldBe Some(emptySubst)
    unifyTypes(SLong, SBoolean) shouldBe None

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
    
    check("Option[Int]", "Option[Boolean]", None)
    check("Option[Int]", "Option[Int]")
    check("Option[(Int,Box)]", "Option[Int]", None)
    check("Option[(Int,Box)]", "Option[(Int,Box)]")
    check("Option[Option[Int]]", "Option[Option[Int]]")

    check("Int => Int", "Int => Boolean", None)
    check("Int => Int", "Int => Int")
    check("(Int, Boolean) => Int", "Int => Int", None)
    check("(Int, Boolean) => Int", "(Int,Boolean) => Int")

    unify("A", "A")
    check("A", "B", None)

    check("(Int, A)", "Int", None)
    unify("(Int, A)", "(Int, A)")
    unify("(Int, A)", "(Int, Int)", ("A", SLong))
    unify("(A, B)", "(A, B)")
    unify("(A, B)", "(Int, Boolean)", ("A", SLong), ("B", SBoolean))
    check("(A, B)", "(Int, Boolean, Box)", None)
    check("(A, Boolean)", "(Int, B)", None)
    check("(A, Int)", "(B, Int)", None)

    unify("A", "Array[Boolean]", ("A", ty("Array[Boolean]")))
    unify("Array[A]", "Array[Int]", ("A", SLong))
    unify("Array[A]", "Array[(Int, Box)]", ("A", ty("(Int, Box)")))
    unify("Array[(Int, A)]", "Array[(Int, Box)]", ("A", SBox))
    unify("Array[Array[A]]", "Array[Array[Int]]", ("A", SLong))
    unify("Array[Array[A]]", "Array[Array[A]]")
    check("Array[Array[A]]", "Array[Array[B]]", None)

    unify("A", "Option[Boolean]", ("A", ty("Option[Boolean]")))
    unify("Option[A]", "Option[Int]", ("A", SLong))
    unify("Option[A]", "Option[(Int, Box)]", ("A", ty("(Int, Box)")))
    unify("Option[(Int, A)]", "Option[(Int, Box)]", ("A", SBox))
    unify("Option[Option[A]]", "Option[Option[Int]]", ("A", SLong))
    unify("Option[Option[A]]", "Option[Option[A]]")
    check("Option[Option[A]]", "Option[Option[B]]", None)

    unify("A => Int", "Int => Int", ("A", SLong))
    check("A => Int", "Int => Boolean", None)
    unify("Int => A", "Int => Int", ("A", SLong))
    check("Int => A", "Boolean => Int", None)
    unify("(Int, A) => B", "(Int, Boolean) => Box", ("A", SBoolean), ("B", SBox))
    check("(Int, A) => A", "(Int, Boolean) => Box", None)
    unify("(Int, A) => A", "(Int, Boolean) => Boolean", ("A", SBoolean))

    unify(
      "((A,Int), Array[B] => Array[(Array[C], B)]) => A",
      "((Int,Int), Array[Boolean] => Array[(Array[C], Boolean)]) => Int",
      ("A", SLong), ("B", SBoolean))
  }
}
