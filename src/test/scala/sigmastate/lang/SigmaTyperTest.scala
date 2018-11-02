package sigmastate.lang

import org.scalatest.prop.PropertyChecks
import org.scalatest.{PropSpec, Matchers}
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate._
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.SigmaPredef._
import sigmastate.lang.exceptions.{NonApplicableMethod, TyperException, InvalidBinaryOperationParameters, MethodNotFound}
import sigmastate.serialization.generators.ValueGenerators

class SigmaTyperTest extends PropSpec with PropertyChecks with Matchers with LangTests with ValueGenerators {

  def typecheck(env: ScriptEnv, x: String): SType = {
    try {
      val builder = TransformingSigmaBuilder
      val parsed = SigmaParser(x, builder).get.value
      val binder = new SigmaBinder(env, builder)
      val bound = binder.bind(parsed)
      val st = new SigmaTree(bound)
      val typer = new SigmaTyper(builder)
      val typed = typer.typecheck(bound)
     typed.tpe
    } catch {
      case e: Exception => throw e
    }
  }

  def typefail(env: ScriptEnv, x: String, messageSubstr: String = ""): Unit = {
    try {
      val builder = TransformingSigmaBuilder
      val parsed = SigmaParser(x, builder).get.value
      val binder = new SigmaBinder(env, builder)
      val bound = binder.bind(parsed)
      val st = new SigmaTree(bound)
      val typer = new SigmaTyper(builder)
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
    typecheck(env, "x + y") shouldBe SInt
    typecheck(env, "x - y") shouldBe SInt
    typecheck(env, "x / y") shouldBe SInt
    typecheck(env, "x % y") shouldBe SInt
    typecheck(env, "c1 && c2") shouldBe SBoolean
    typecheck(env, "arr1") shouldBe SByteArray
    typecheck(env, "HEIGHT") shouldBe SLong
    typecheck(env, "HEIGHT + 1") shouldBe SLong
    typecheck(env, "INPUTS") shouldBe SCollection(SBox)
    typecheck(env, "INPUTS.size") shouldBe SInt
    typecheck(env, "INPUTS.size > 1") shouldBe SBoolean
    typecheck(env, "arr1 | arr2") shouldBe SByteArray
    typecheck(env, "arr1 ++ arr2") shouldBe SByteArray
    typecheck(env, "col1 ++ col2") shouldBe SCollection(SLong)
    typecheck(env, "g1 ^ n1") shouldBe SGroupElement
    typecheck(env, "g1 * g2") shouldBe SGroupElement
    typecheck(env, "p1 || p2") shouldBe SBoolean
    typecheck(env, "p1 && p2") shouldBe SBoolean
    typecheck(env, "b1 < b2") shouldBe SBoolean
    typecheck(env, "b1 > b2") shouldBe SBoolean
    typecheck(env, "b1 <= b2") shouldBe SBoolean
    typecheck(env, "b1 >= b2") shouldBe SBoolean
    typecheck(env, "n1 < n2") shouldBe SBoolean
    typecheck(env, "n1 > n2") shouldBe SBoolean
    typecheck(env, "n1 <= n2") shouldBe SBoolean
    typecheck(env, "n1 >= n2") shouldBe SBoolean
    typecheck(env, "n1 == n2") shouldBe SBoolean
    typecheck(env, "n1 != n2") shouldBe SBoolean
  }

  property("predefined functions") {
    typecheck(env, "allOf") shouldBe AllSym.tpe
    typecheck(env, "allOf(Array(c1, c2))") shouldBe SBoolean
    typecheck(env, "getVar[Byte](10).get") shouldBe SByte
    typecheck(env, "getVar[Array[Byte]](10).get") shouldBe SByteArray
    typecheck(env, "getVar[SigmaProp](10).get") shouldBe SSigmaProp
    typecheck(env, "p1 && getVar[SigmaProp](10).get") shouldBe SBoolean
    typecheck(env, "getVar[SigmaProp](10).get || p2") shouldBe SBoolean
    typecheck(env, "getVar[SigmaProp](10).get && getVar[SigmaProp](11).get") shouldBe SBoolean
    typecheck(env, "Array(true, getVar[SigmaProp](11).get)") shouldBe SCollection(SBoolean)
    typecheck(env, "min(1, 2)") shouldBe SInt
    typecheck(env, "min(1L, 2)") shouldBe SLong
    typecheck(env, "min(HEIGHT, INPUTS.size)") shouldBe SLong
    typecheck(env, "max(1, 2)") shouldBe SInt
    typecheck(env, "max(1L, 2)") shouldBe SLong
    typecheck(env, """fromBase58("111")""") shouldBe SByteArray
    typecheck(env, """fromBase64("111")""") shouldBe SByteArray
    typecheck(env, """PK("111")""") shouldBe SSigmaProp
  }

  property("val constructs") {
    typecheck(env, "{val X = 10; X > 2}") shouldBe SBoolean
    typecheck(env, """{val X = 10; X >= X}""".stripMargin) shouldBe SBoolean
    typecheck(env, """{val X = 10 + 1; X >= X}""".stripMargin) shouldBe SBoolean
    typecheck(env,
      """{val X = 10
       |val Y = X + 1
       |X < Y}
      """.stripMargin) shouldBe SBoolean
    typecheck(env, "{val X = (10, true); X._1 > 2 && X._2}") shouldBe SBoolean
    typecheck(env, "{val X = (Array(1,2,3), 1); X}") shouldBe STuple(SCollection(SInt), SInt)
  }

  property("generic methods of arrays") {
    val minToRaise = LongConstant(1000)
    val env = this.env ++ Map(
      "minToRaise" -> minToRaise
    )
    typecheck(env, "OUTPUTS.map({ (out: Box) => out.value >= minToRaise })") shouldBe ty("Array[Boolean]")
    typecheck(env, "OUTPUTS.exists({ (out: Box) => out.value >= minToRaise })") shouldBe SBoolean
    typecheck(env, "OUTPUTS.forall({ (out: Box) => out.value >= minToRaise })") shouldBe SBoolean
    typecheck(env, "{ val arr = Array(1,2,3); arr.fold(0, { (i1: Int, i2: Int) => i1 + i2 })}") shouldBe SInt
    typecheck(env, "OUTPUTS.slice(0, 10)") shouldBe ty("Array[Box]")
    typecheck(env, "OUTPUTS.where({ (out: Box) => out.value >= minToRaise })") shouldBe ty("Array[Box]")
  }

  property("tuple constructor") {
    typecheck(env, "()") shouldBe SUnit
    typecheck(env, "(1)") shouldBe SInt
    typecheck(env, "(1, 2)") shouldBe STuple(SInt, SInt)
    typecheck(env, "(1, x + 1)") shouldBe STuple(SInt, SInt)
    typecheck(env, "(1, 2, 3)") shouldBe STuple(SInt, SInt, SInt)
    typecheck(env, "(1, 2 + 3, 4)") shouldBe STuple(SInt, SInt, SInt)

    typecheck(env, "(1, 2L)._1") shouldBe SInt
    typecheck(env, "(1, 2L)._2") shouldBe SLong
    typecheck(env, "(1, 2L, 3)._3") shouldBe SInt

    an[MethodNotFound] should be thrownBy typecheck(env, "(1, 2L)._3")

    // tuple as collection
    typecheck(env, "(1, 2L).size") shouldBe SInt
    typecheck(env, "(1, 2L)(0)") shouldBe SInt
    typecheck(env, "(1, 2L)(1)") shouldBe SLong
    typecheck(env, "(1, 2L).getOrElse(2, 3)") shouldBe SAny
    typecheck(env, "(1, 2L).slice(0, 2)") shouldBe SCollection(SAny)
    typecheck(env, "{ (a: Int) => (1, 2L)(a) }") shouldBe SFunc(IndexedSeq(SInt), SAny)
  }

  property("types") {
    typecheck(env, "{val X: Int = 10; 3 > 2}") shouldBe SBoolean
    typecheck(env, "{val X: (Int, Boolean) = (10, true); 3 > 2}") shouldBe SBoolean
    typecheck(env, "{val X: Array[Int] = Array(1,2,3); X.size}") shouldBe SInt
    typecheck(env, "{val X: (Array[Int], Int) = (Array(1,2,3), 1); X}") shouldBe STuple(SCollection(SInt), SInt)
    typecheck(env, "{val X: (Array[Int], Int) = (Array(1,2,3), x); X._1}") shouldBe SCollection(SInt)
    typecheck(env, "{val X: (Array[Int], Int) = (Array(1,2,3), x); X._1}") shouldBe SCollection(SInt)
  }

  property("if") {
    typecheck(env, "if(true) 1 else 2") shouldBe SInt
    typecheck(env, "if(c1) 1 else 2") shouldBe SInt
    typecheck(env, "if(c1) x else y") shouldBe SInt
    typecheck(env,
      """if (true) {
        |  val A = 10; A
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

  property("methods returning Option") {
    typecheck(env, "getVar[Int](10)") shouldBe SOption(SInt)
    typecheck(env, "{ val v = getVar[Int](1); v.get }") shouldBe SInt
  }

  property("array indexed access") {
    typefail(env, "Array()(0)", "Undefined type of empty collection")
    typecheck(env, "Array(0)(0)") shouldBe SInt
    typefail(env, "Array(0)(0)(0)", "array type is expected")
  }

  property("array indexed access with evaluation") {
    typecheck(env, "Array(0)(1 - 1)") shouldBe SInt
    typecheck(env, "Array(0)((1 - 1) + 0)") shouldBe SInt
    typefail(env, "Array(0)(0 == 0)", "Invalid argument type of array application")
    typefail(env, "Array(0)(1,1,1)", "Invalid argument of array application")
  }

  property("array indexed access with default value") {
    typecheck(env, "Array(0).getOrElse(0, 1)") shouldBe SInt
    typefail(env, "Array(0).getOrElse(true, 1)", "Invalid argument type of application")
    typefail(env, "Array(true).getOrElse(0, 1)", "Invalid argument type of application")
    typefail(env, "Array(0).getOrElse(0, Array(1))", "Invalid argument type of application")
  }

  property("array indexed access with default value with evaluation") {
    typecheck(env, "Array(0).getOrElse(0, (2 - 1) + 0)") shouldBe SInt
  }

  property("lambdas") {
    typecheck(env, "{ (a: Int) => a + 1 }") shouldBe SFunc(IndexedSeq(SInt), SInt)
    typecheck(env, "{ (a: Int) => a + 1 }") shouldBe SFunc(IndexedSeq(SInt), SInt)
    typecheck(env, "{ (a: Int) => { a + 1 } }") shouldBe SFunc(IndexedSeq(SInt), SInt)
    typecheck(env, "{ (a: Int) => { val b = a + 1; b } }") shouldBe SFunc(IndexedSeq(SInt), SInt)
    typecheck(env, "{ (a: Int, box: Box) => a + box.value }") shouldBe
      SFunc(IndexedSeq(SInt, SBox), SLong)
    typecheck(env, "{ (p: (Int, GroupElement), box: Box) => p._1 > box.value && p._2.isIdentity }") shouldBe
      SFunc(IndexedSeq(STuple(SInt, SGroupElement), SBox), SBoolean)
    typecheck(env, "{ (p: (Int, SigmaProp), box: Box) => p._1 > box.value && p._2.isValid }") shouldBe
      SFunc(IndexedSeq(STuple(SInt, SSigmaProp), SBox), SBoolean)

    typefail(env, "{ (a) => a + 1 }", "undefined type of argument")
  }

  property("function definitions") {
    typecheck(env, "{ val f = { (x: Int) => x + 1 }; f }") shouldBe SFunc(IndexedSeq(SInt), SInt)
  }

  property("predefined primitives") {
    typecheck(env, "{ (box: Box) => box.value }") shouldBe SFunc(IndexedSeq(SBox), SLong)
    typecheck(env, "{ (box: Box) => box.propositionBytes }") shouldBe SFunc(IndexedSeq(SBox), SByteArray)
    typecheck(env, "{ (box: Box) => box.bytes }") shouldBe SFunc(IndexedSeq(SBox), SByteArray)
    typecheck(env, "{ (box: Box) => box.id }") shouldBe SFunc(IndexedSeq(SBox), SByteArray)
  }

  property("type parameters") {
    typecheck(env, "SELF.R1[Int]") shouldBe SOption(SInt)
    typecheck(env, "SELF.R1[Int].isDefined") shouldBe SBoolean
    typecheck(env, "SELF.R1[Int].isEmpty") shouldBe SBoolean
    typecheck(env, "SELF.R1[Int].get") shouldBe SInt
    typefail(env, "x[Int]", "doesn't have type parameters")
    typefail(env, "arr1[Int]", "doesn't have type parameters")
    typecheck(env, "SELF.R1[(Int,Boolean)]") shouldBe SOption(STuple(SInt, SBoolean))
    typecheck(env, "SELF.R1[(Int,Boolean)].get") shouldBe STuple(SInt, SBoolean)
    an[IllegalArgumentException] should be thrownBy typecheck(env, "SELF.R1[Int,Boolean].get")
    typecheck(env, "Array[Int]()") shouldBe SCollection(SInt)
  }


  property("compute unifying type substitution: prim types") {
    import SigmaTyper._
    forAll { t: SPredefType =>
      unifyTypes(t, t) shouldBe Some(emptySubst)
      unifyTypes(SAny, t) shouldBe Some(emptySubst)
      unifyTypes(SAny, SCollection(t)) shouldBe Some(emptySubst)
      unifyTypes(SCollection(SAny), SCollection(t)) shouldBe Some(emptySubst)
      unifyTypes(SCollection(SAny), STuple(t, t, t)) shouldBe Some(emptySubst)
      unifyTypes(SCollection(SAny), STuple(t, STuple(t, t))) shouldBe Some(emptySubst)
    }
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
    unifyTypes(SLong, SBoolean) shouldBe None

    check("(Int, Boolean)", "Int", None)
    check("(Int, Boolean)", "(Int, Boolean)")
    check("(Int, Boolean)", "(Int, Int)", None)
    check("(Int, Box)", "(Int, Box)")
    check("(Int, Box)", "(Int, Box, Boolean)", None)

    check("Array[Any]", "(Int, Long)")  // tuple as array
    check("Array[Array[Any]]", "Array[(Int, Long)]")

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

    unifyTypes(SBoolean, SSigmaProp) shouldBe Some(emptySubst)
    unifyTypes(SSigmaProp, SBoolean) shouldBe None
    check("(Int, Boolean)", "(Int, SigmaProp)")
    check("(Int, Boolean, Boolean)", "(Int, SigmaProp, SigmaProp)")
    check("Array[Boolean]", "Array[SigmaProp]")
    check("Array[(Int,Boolean)]", "Array[(Int,SigmaProp)]")
    check("Array[Array[Boolean]]", "Array[Array[SigmaProp]]")
    check("Option[Boolean]", "Option[SigmaProp]")
    check("Option[(Int,Boolean)]", "Option[(Int,SigmaProp)]")
    check("Option[Option[Boolean]]", "Option[Option[SigmaProp]]")
    check("Int => Boolean", "Int => SigmaProp")
    check("(Int, Boolean) => Int", "(Int, SigmaProp) => Int")
  }

  property("most specific general (MSG) type") {
    import SigmaTyper._
    def checkTypes(t1: SType, t2: SType, exp: Option[SType]): Unit = {
      msgType(t1, t2) shouldBe exp
    }
    def checkAllTypes(ts: Seq[SType], exp: Option[SType]): Unit = {
      msgTypeOf(ts) shouldBe exp
    }
    def check(s1: String, s2: String, exp: Option[SType]): Unit = {
      val t1 = ty(s1); val t2 = ty(s2)
      checkTypes(t1, t2, exp)
    }
    def checkAll(ts: Seq[String], exp: Option[SType]): Unit = {
      val types = ts.map(ty(_));
      checkAllTypes(types, exp)
    }

    checkTypes(NoType, NoType, None)
    checkTypes(NoType, SInt, None)
    checkTypes(SInt, SInt, Some(SInt))
    checkTypes(SBoolean, SSigmaProp, Some(SBoolean))
    checkTypes(SSigmaProp, SBoolean, Some(SBoolean))

    check("(Int, Boolean)", "(Int, SigmaProp)", Some(ty("(Int, Boolean)")))
    check("(Int, SigmaProp)", "(Int, Boolean)", Some(ty("(Int, Boolean)")))
    check("Array[Boolean]", "Array[SigmaProp]", Some(ty("Array[Boolean]")))
    check("Array[SigmaProp]", "Array[Boolean]", Some(ty("Array[Boolean]")))
    check("Array[(Int,Boolean)]", "Array[(Int,SigmaProp)]", Some(ty("Array[(Int,Boolean)]")))
    check("Array[(Int,SigmaProp)]", "Array[(Int,Boolean)]", Some(ty("Array[(Int,Boolean)]")))
    check("Array[Array[Boolean]]", "Array[Array[SigmaProp]]", Some(ty("Array[Array[Boolean]]")))
    check("Array[Array[SigmaProp]]", "Array[Array[Boolean]]", Some(ty("Array[Array[Boolean]]")))
    check("Option[(Int,Boolean)]", "Option[(Int,SigmaProp)]", Some(ty("Option[(Int,Boolean)]")))
    check("Option[(Int,SigmaProp)]", "Option[(Int,Boolean)]", Some(ty("Option[(Int,Boolean)]")))
    check("Option[Option[Boolean]]", "Option[Option[SigmaProp]]", Some(ty("Option[Option[Boolean]]")))
    check("Option[Option[SigmaProp]]", "Option[Option[Boolean]]", Some(ty("Option[Option[Boolean]]")))
    check("Int => Boolean", "Int => SigmaProp", Some(ty("Int => Boolean")))
    check("Int => SigmaProp", "Int => Boolean", Some(ty("Int => Boolean")))

    checkAll(Seq("Boolean", "SigmaProp"), Some(SBoolean))
    checkAll(Seq("Boolean", "SigmaProp", "Boolean"), Some(SBoolean))
    checkAll(Seq("Boolean", "SigmaProp", "Int"), None)
    checkAll(Seq("Int", "Int", "Int"), Some(SInt))
    checkAll(Seq("(Int, Boolean)", "(Int,SigmaProp)", "(Int,Boolean)"), Some(ty("(Int,Boolean)")))
  }

  property("invalid binary operations type check") {
    an[InvalidBinaryOperationParameters] should be thrownBy typecheck(env, "1 == false")
    an[InvalidBinaryOperationParameters] should be thrownBy typecheck(env, "1 != false")
    an[InvalidBinaryOperationParameters] should be thrownBy typecheck(env, "1 > false")
    an[InvalidBinaryOperationParameters] should be thrownBy typecheck(env, "1 >= false")
    an[InvalidBinaryOperationParameters] should be thrownBy typecheck(env, "1 < false")
    an[InvalidBinaryOperationParameters] should be thrownBy typecheck(env, "1 <= false")
    an[InvalidBinaryOperationParameters] should be thrownBy typecheck(env, "1 + false")
    an[InvalidBinaryOperationParameters] should be thrownBy typecheck(env, "1 - false")
    an[InvalidBinaryOperationParameters] should be thrownBy typecheck(env, "1 / false")
    an[InvalidBinaryOperationParameters] should be thrownBy typecheck(env, "1 % false")
    an[InvalidBinaryOperationParameters] should be thrownBy typecheck(env, "min(1, false)")
    an[InvalidBinaryOperationParameters] should be thrownBy typecheck(env, "max(1, false)")
    an[InvalidBinaryOperationParameters] should be thrownBy typecheck(env, "1 * false")
    an[InvalidBinaryOperationParameters] should be thrownBy typecheck(env, "1 + \"a\"")
    an[NonApplicableMethod] should be thrownBy typecheck(env, "1 || 1")
    an[NonApplicableMethod] should be thrownBy typecheck(env, "col1 || col2")
    an[NonApplicableMethod] should be thrownBy typecheck(env, "g1 || g2")
    an[NonApplicableMethod] should be thrownBy typecheck(env, "true ++ false")
    an[NonApplicableMethod] should be thrownBy typecheck(env, "\"a\" ++ \"a\"")
  }

  property("upcast for binary operations with numeric types") {
    typecheck(env, "1 == 1L") shouldBe SBoolean
    typecheck(env, "1 > 1L") shouldBe SBoolean
    typecheck(env, "1 >= 1L") shouldBe SBoolean
    typecheck(env, "1 < 1L") shouldBe SBoolean
    typecheck(env, "1 <= 1L") shouldBe SBoolean
    typecheck(env, "1 + 1L") shouldBe SLong
    typecheck(env, "1 - 1L") shouldBe SLong
    typecheck(env, "1 * 1L") shouldBe SLong
    typecheck(env, "1 / 1L") shouldBe SLong
    typecheck(env, "1 % 1L") shouldBe SLong
  }

  property("casts for numeric types") {
    typecheck(env, "1.toByte") shouldBe SByte
    typecheck(env, "1.toShort") shouldBe SShort
    typecheck(env, "1L.toInt") shouldBe SInt
    typecheck(env, "1.toLong") shouldBe SLong
    typecheck(env, "1.toBigInt") shouldBe SBigInt
    // in an expression
    typecheck(env, "1L * 1.toLong") shouldBe SLong
  }

  property("invalid cast method for numeric types") {
    an[MethodNotFound] should be thrownBy typecheck(env, "1.toSuperBigInteger")
  }

  property("string concat") {
    typecheck(env, """ "a" + "b" """) shouldBe SString
  }
}
