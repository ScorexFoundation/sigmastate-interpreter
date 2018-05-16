package sigmastate.lang

import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import sigmastate._
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate.lang.SigmaPredef._
import sigmastate.utxo.{Outputs, Self, Inputs}

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
    typecheck(env, "col1 ++ col2") shouldBe SCollection(SInt)
    typecheck(env, "g1 ^ n") shouldBe SGroupElement
    typecheck(env, "g1 * g2") shouldBe SGroupElement
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
    typecheck(env, "{let X = (Array(1,2,3), 1); X}") shouldBe STuple(SCollection(SInt), SInt)
  }

  property("generic methods of arrays") {
    val minToRaise = IntConstant(1000)
    val env = this.env ++ Map(
      "minToRaise" -> minToRaise
    )
    typecheck(env, "OUTPUTS.map(fun (out: Box) = { out.value >= minToRaise })") shouldBe ty("Array[Boolean]")
    typecheck(env, "OUTPUTS.exists(fun (out: Box) = { out.value >= minToRaise })") shouldBe SBoolean
    typecheck(env, "OUTPUTS.forall(fun (out: Box) = { out.value >= minToRaise })") shouldBe SBoolean
    typecheck(env, "{ let arr = Array(1,2,3); arr.fold(0, fun (n1: Int, n2: Int) = n1 + n2)}") shouldBe SInt
    typecheck(env, "OUTPUTS.slice(0, 10)") shouldBe ty("Array[Box]")
    typecheck(env, "OUTPUTS.where(fun (out: Box) = { out.value >= minToRaise })") shouldBe ty("Array[Box]")
  }

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
    typefail(env, "Array()", "Undefined type of empty collection")
    typefail(env, "Array(Array())", "Undefined type of empty collection")
    typefail(env, "Array(Array(Array()))", "Undefined type of empty collection")

    typecheck(env, "Array(1)") shouldBe SCollection(SInt)
    typecheck(env, "Array(1, x)") shouldBe SCollection(SInt)
    typecheck(env, "Array(Array(x + 1))") shouldBe SCollection(SCollection(SInt))

    typefail(env, "Array(1, x + 1, Array())")
    typefail(env, "Array(1, false)")
  }

  property("Option constructors") {
    typecheck(env, "Some(10)") shouldBe SOption(SInt)
    typecheck(env, "Some(x)") shouldBe SOption(SInt)
    typecheck(env, "Some(x + 1)") shouldBe SOption(SInt)
    typecheck(env, "Some(Some(10))") shouldBe SOption(SOption(SInt))
  }

  property("array indexed access") {
    typefail(env, "Array()(0)", "Undefined type of empty collection")
    typecheck(env, "Array(0)(0)") shouldBe SInt
    typefail(env, "Array(0)(0)(0)", "array type is expected")
  }

  property("lambdas") {
    typecheck(env, "fun (a: Int) = a + 1") shouldBe SFunc(IndexedSeq(SInt), SInt)
    typecheck(env, "fun (a: Int): Int = a + 1") shouldBe SFunc(IndexedSeq(SInt), SInt)
    typecheck(env, "fun (a: Int) = { a + 1 }") shouldBe SFunc(IndexedSeq(SInt), SInt)
    typecheck(env, "fun (a: Int) = { let b = a + 1; b }") shouldBe SFunc(IndexedSeq(SInt), SInt)
    typecheck(env, "fun (a: Int, box: Box): Int = a + box.value") shouldBe
      SFunc(IndexedSeq(SInt, SBox), SInt)
    typecheck(env, "fun (p: (Int, GroupElement), box: Box): Boolean = p._1 > box.value && p._2.isIdentity") shouldBe
      SFunc(IndexedSeq(STuple(SInt, SGroupElement), SBox), SBoolean)

    typefail(env, "fun (a) = a + 1", "undefined type of argument")
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

  property("type parameters") {
    typecheck(env, "SELF.R1[Int]") shouldBe SOption(SInt)
    typecheck(env, "SELF.R1[Int].isDefined") shouldBe SBoolean
    typecheck(env, "SELF.R1[Int].value") shouldBe SInt
    typefail(env, "X[Int]", "expression doesn't have type parameters")
    typefail(env, "arr1[Int]", "expression doesn't have type parameters")
    typecheck(env, "SELF.R1[(Int,Boolean)]") shouldBe SOption(STuple(SInt, SBoolean))
    typecheck(env, "SELF.R1[(Int,Boolean)].value") shouldBe STuple(SInt, SBoolean)
    typefail(env, "SELF.R1[Int,Boolean].value", "Wrong number of type arguments")
    typecheck(env, "Array[Int]()") shouldBe SCollection(SInt)
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
    unify("(Int, A)", "(Int, Int)", ("A", SInt))
    unify("(A, B)", "(A, B)")
    unify("(A, B)", "(Int, Boolean)", ("A", SInt), ("B", SBoolean))
    check("(A, B)", "(Int, Boolean, Box)", None)
    check("(A, Boolean)", "(Int, B)", None)
    check("(A, Int)", "(B, Int)", None)

    unify("A", "Array[Boolean]", ("A", ty("Array[Boolean]")))
    unify("Array[A]", "Array[Int]", ("A", SInt))
    unify("Array[A]", "Array[(Int, Box)]", ("A", ty("(Int, Box)")))
    unify("Array[(Int, A)]", "Array[(Int, Box)]", ("A", SBox))
    unify("Array[Array[A]]", "Array[Array[Int]]", ("A", SInt))
    unify("Array[Array[A]]", "Array[Array[A]]")
    check("Array[Array[A]]", "Array[Array[B]]", None)

    unify("A", "Option[Boolean]", ("A", ty("Option[Boolean]")))
    unify("Option[A]", "Option[Int]", ("A", SInt))
    unify("Option[A]", "Option[(Int, Box)]", ("A", ty("(Int, Box)")))
    unify("Option[(Int, A)]", "Option[(Int, Box)]", ("A", SBox))
    unify("Option[Option[A]]", "Option[Option[Int]]", ("A", SInt))
    unify("Option[Option[A]]", "Option[Option[A]]")
    check("Option[Option[A]]", "Option[Option[B]]", None)

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
      ("A", SInt), ("B", SBoolean))
  }

}
