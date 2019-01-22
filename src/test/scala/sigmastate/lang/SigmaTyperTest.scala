package sigmastate.lang

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import org.ergoplatform.{ErgoAddressEncoder, Height, Inputs}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate._
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.SigmaPredef._
import sigmastate.lang.Terms.{Ident, Select}
import sigmastate.lang.exceptions.{InvalidBinaryOperationParameters, MethodNotFound, NonApplicableMethod, TyperException}
import sigmastate.serialization.generators.ValueGenerators
import sigmastate.utxo.{Append, ExtractCreationInfo, SizeOf}

class SigmaTyperTest extends PropSpec with PropertyChecks with Matchers with LangTests with ValueGenerators {

  private val predefFuncRegistry = new PredefinedFuncRegistry(StdSigmaBuilder)
  import predefFuncRegistry._

  def typecheck(env: ScriptEnv, x: String, expected: SValue = null): SType = {
    try {
      val builder = TransformingSigmaBuilder
      val parsed = SigmaParser(x, builder).get.value
      val predefinedFuncRegistry = new PredefinedFuncRegistry(builder)
      val binder = new SigmaBinder(env, builder, TestnetNetworkPrefix, predefinedFuncRegistry)
      val bound = binder.bind(parsed)
      val st = new SigmaTree(bound)
      val typer = new SigmaTyper(builder, predefinedFuncRegistry)
      val typed = typer.typecheck(bound)
      if (expected != null) typed shouldBe expected
      typed.tpe
    } catch {
      case e: Exception => throw e
    }
  }

  def typefail(env: ScriptEnv, x: String, messageSubstr: String = ""): Unit = {
    try {
      val builder = TransformingSigmaBuilder
      val parsed = SigmaParser(x, builder).get.value
      val predefinedFuncRegistry = new PredefinedFuncRegistry(builder)
      val binder = new SigmaBinder(env, builder, TestnetNetworkPrefix, predefinedFuncRegistry)
      val bound = binder.bind(parsed)
      val st = new SigmaTree(bound)
      val typer = new SigmaTyper(builder, predefinedFuncRegistry)
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
    typecheck(env, "x", IntConstant(10)) shouldBe SInt   // constants are substituted from env
    typecheck(env, "x + y", Plus(10, 11)) shouldBe SInt
    typecheck(env, "x + height1", Plus(Upcast(10, SLong), 100L)) shouldBe SLong
    typecheck(env, "x - y") shouldBe SInt
    typecheck(env, "x / y") shouldBe SInt
    typecheck(env, "x % y") shouldBe SInt
    typecheck(env, "c1 && c2", BinAnd(TrueLeaf, FalseLeaf)) shouldBe SBoolean
    typecheck(env, "arr1", ByteArrayConstant(Array[Byte](1,2))) shouldBe SByteArray
    typecheck(env, "HEIGHT", Height) shouldBe SInt
    typecheck(env, "HEIGHT + 1") shouldBe SInt
    typecheck(env, "INPUTS", Inputs) shouldBe SCollection(SBox)
    typecheck(env, "INPUTS.size") shouldBe SInt
    typecheck(env, "INPUTS.size > 1", GT(Select(Inputs, "size", Some(SInt)), 1)) shouldBe SBoolean
    // todo: restore in https://github.com/ScorexFoundation/sigmastate-interpreter/issues/324
//    typecheck(env, "arr1 | arr2", Xor(ByteArrayConstant(arr1), ByteArrayConstant(arr2))) shouldBe SByteArray
    typecheck(env, "arr1 ++ arr2", Append(ByteArrayConstant(arr1), ByteArrayConstant(arr2))) shouldBe SByteArray
    typecheck(env, "col1 ++ col2") shouldBe SCollection(SLong)
    // todo should be g1.exp(n1)
    // ( see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/324 )
//    typecheck(env, "g1 ^ n1") shouldBe SGroupElement
    typecheck(env, "g1 * g2") shouldBe SGroupElement
    typecheck(env, "p1 || p2") shouldBe SSigmaProp
    typecheck(env, "p1 && p2") shouldBe SSigmaProp
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
    typecheck(env, "allOf") shouldBe AllOfFunc.declaration.tpe
    typecheck(env, "allOf(Coll(c1, c2))") shouldBe SBoolean
    typecheck(env, "getVar[Byte](10).get") shouldBe SByte
    typecheck(env, "getVar[Coll[Byte]](10).get") shouldBe SByteArray
    typecheck(env, "getVar[SigmaProp](10).get") shouldBe SSigmaProp
    typecheck(env, "p1 && getVar[SigmaProp](10).get") shouldBe SSigmaProp
    typecheck(env, "getVar[SigmaProp](10).get || p2") shouldBe SSigmaProp
    typecheck(env, "getVar[SigmaProp](10).get && getVar[SigmaProp](11).get") shouldBe SSigmaProp
    typecheck(env, "Coll(true, getVar[SigmaProp](11).get)") shouldBe SCollection(SBoolean)
    typecheck(env, "min(1, 2)") shouldBe SInt
    typecheck(env, "min(1L, 2)") shouldBe SLong
    typecheck(env, "min(HEIGHT, INPUTS.size)") shouldBe SInt
    typecheck(env, "max(1, 2)") shouldBe SInt
    typecheck(env, "max(1L, 2)") shouldBe SLong
    typecheck(env, """fromBase58("111")""") shouldBe SByteArray
    typecheck(env, """fromBase64("111")""") shouldBe SByteArray
    typecheck(env, """PK("tJPvNjccEZZF2Cwb6WNsRFmUa79Dy3npbmnfUKnBRREq2cuaULCo2R")""") shouldBe SSigmaProp
    typecheck(env, "sigmaProp(HEIGHT > 1000)") shouldBe SSigmaProp
    typecheck(env, "ZKProof { sigmaProp(HEIGHT > 1000) }") shouldBe SBoolean
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
    typecheck(env, "{val X = (Coll(1,2,3), 1); X}") shouldBe STuple(SCollection(SInt), SInt)
  }

  property("generic methods of arrays") {
    val minToRaise = LongConstant(1000)
    val env = this.env ++ Map(
      "minToRaise" -> minToRaise
    )
    typecheck(env, "OUTPUTS.map({ (out: Box) => out.value >= minToRaise })") shouldBe ty("Coll[Boolean]")
    typecheck(env, "OUTPUTS.exists({ (out: Box) => out.value >= minToRaise })") shouldBe SBoolean
    typecheck(env, "OUTPUTS.forall({ (out: Box) => out.value >= minToRaise })") shouldBe SBoolean
    typecheck(env, "{ val arr = Coll(1,2,3); arr.fold(0, { (i1: Int, i2: Int) => i1 + i2 })}") shouldBe SInt
    typecheck(env, "OUTPUTS.slice(0, 10)") shouldBe ty("Coll[Box]")
    typecheck(env, "OUTPUTS.filter({ (out: Box) => out.value >= minToRaise })") shouldBe ty("Coll[Box]")
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
    typecheck(env, "{val X: Coll[Int] = Coll(1,2,3); X.size}") shouldBe SInt
    typecheck(env, "{val X: (Coll[Int], Int) = (Coll(1,2,3), 1); X}") shouldBe STuple(SCollection(SInt), SInt)
    typecheck(env, "{val X: (Coll[Int], Int) = (Coll(1,2,3), x); X._1}") shouldBe SCollection(SInt)
    typecheck(env, "{val X: (Coll[Int], Int) = (Coll(1,2,3), x); X._1}") shouldBe SCollection(SInt)
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
    typefail(env, "Coll()", "Undefined type of empty collection")
    typefail(env, "Coll(Coll())", "Undefined type of empty collection")
    typefail(env, "Coll(Coll(Coll()))", "Undefined type of empty collection")

    typecheck(env, "Coll(1)") shouldBe SCollection(SInt)
    typecheck(env, "Coll(1, x)") shouldBe SCollection(SInt)
    typecheck(env, "Coll(Coll(x + 1))") shouldBe SCollection(SCollection(SInt))

    typefail(env, "Coll(1, x + 1, Coll())")
    typefail(env, "Coll(1, false)")
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
    typefail(env, "Coll()(0)", "Undefined type of empty collection")
    typecheck(env, "Coll(0)(0)") shouldBe SInt
    typefail(env, "Coll(0)(0)(0)", "array type is expected")
  }

  property("array indexed access with evaluation") {
    typecheck(env, "Coll(0)(1 - 1)") shouldBe SInt
    typecheck(env, "Coll(0)((1 - 1) + 0)") shouldBe SInt
    typefail(env, "Coll(0)(0 == 0)", "Invalid argument type of array application")
    typefail(env, "Coll(0)(1,1,1)", "Invalid argument of array application")
  }

  property("array indexed access with default value") {
    typecheck(env, "Coll(0).getOrElse(0, 1)") shouldBe SInt
    typefail(env, "Coll(0).getOrElse(true, 1)", "Invalid argument type of application")
    typefail(env, "Coll(true).getOrElse(0, 1)", "Invalid argument type of application")
    typefail(env, "Coll(0).getOrElse(0, Coll(1))", "Invalid argument type of application")
  }

  property("array indexed access with default value with evaluation") {
    typecheck(env, "Coll(0).getOrElse(0, (2 - 1) + 0)") shouldBe SInt
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
    typecheck(env, "{ (p: (Int, SigmaProp), box: Box) => p._1 > box.value && p._2.isProven }") shouldBe
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
    typecheck(env, "{ (box: Box) => box.creationInfo }") shouldBe
      SFunc(IndexedSeq(SBox), ExtractCreationInfo.ResultType)
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
    typecheck(env, "Coll[Int]()") shouldBe SCollection(SInt)
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

    check("Coll[Any]", "(Int, Long)")  // tuple as array
    check("Coll[Coll[Any]]", "Coll[(Int, Long)]")

    check("Coll[Int]", "Coll[Boolean]", None)
    check("Coll[Int]", "Coll[Int]")
    check("Coll[(Int,Box)]", "Coll[Int]", None)
    check("Coll[(Int,Box)]", "Coll[(Int,Box)]")
    check("Coll[Coll[Int]]", "Coll[Coll[Int]]")
    
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

    unify("A", "Coll[Boolean]", ("A", ty("Coll[Boolean]")))
    unify("Coll[A]", "Coll[Int]", ("A", SInt))
    unify("Coll[A]", "Coll[(Int, Box)]", ("A", ty("(Int, Box)")))
    unify("Coll[(Int, A)]", "Coll[(Int, Box)]", ("A", SBox))
    unify("Coll[Coll[A]]", "Coll[Coll[Int]]", ("A", SInt))
    unify("Coll[Coll[A]]", "Coll[Coll[A]]")
    check("Coll[Coll[A]]", "Coll[Coll[B]]", None)

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
      "((A,Int), Coll[B] => Coll[(Coll[C], B)]) => A",
      "((Int,Int), Coll[Boolean] => Coll[(Coll[C], Boolean)]) => Int",
      ("A", SInt), ("B", SBoolean))

    unifyTypes(SBoolean, SSigmaProp) shouldBe Some(emptySubst)
    unifyTypes(SSigmaProp, SBoolean) shouldBe None
    check("(Int, Boolean)", "(Int, SigmaProp)")
    check("(Int, Boolean, Boolean)", "(Int, SigmaProp, SigmaProp)")
    check("Coll[Boolean]", "Coll[SigmaProp]")
    check("Coll[(Int,Boolean)]", "Coll[(Int,SigmaProp)]")
    check("Coll[Coll[Boolean]]", "Coll[Coll[SigmaProp]]")
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
    check("Coll[Boolean]", "Coll[SigmaProp]", Some(ty("Coll[Boolean]")))
    check("Coll[SigmaProp]", "Coll[Boolean]", Some(ty("Coll[Boolean]")))
    check("Coll[(Int,Boolean)]", "Coll[(Int,SigmaProp)]", Some(ty("Coll[(Int,Boolean)]")))
    check("Coll[(Int,SigmaProp)]", "Coll[(Int,Boolean)]", Some(ty("Coll[(Int,Boolean)]")))
    check("Coll[Coll[Boolean]]", "Coll[Coll[SigmaProp]]", Some(ty("Coll[Coll[Boolean]]")))
    check("Coll[Coll[SigmaProp]]", "Coll[Coll[Boolean]]", Some(ty("Coll[Coll[Boolean]]")))
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

  property("modular arith ops") {
    typecheck(env, "10.toBigInt.modQ") shouldBe SBigInt
    typecheck(env, "10.toBigInt.plusModQ(2.toBigInt)") shouldBe SBigInt
    typecheck(env, "10.toBigInt.minusModQ(2.toBigInt)") shouldBe SBigInt
    an[MethodNotFound] should be thrownBy typecheck(env, "10.modQ")
    an[TyperException] should be thrownBy typecheck(env, "10.toBigInt.plusModQ(1)")
    an[TyperException] should be thrownBy typecheck(env, "10.toBigInt.minusModQ(1)")
  }

  property("byteArrayToLong") {
    typecheck(env, "byteArrayToLong(Coll[Byte](1.toByte))") shouldBe SLong
    an[TyperException] should be thrownBy typecheck(env, "byteArrayToLong(Coll[Int](1))")
  }

  property("decodePoint") {
    typecheck(env, "decodePoint(Coll[Byte](1.toByte))") shouldBe SGroupElement
    an[TyperException] should be thrownBy typecheck(env, "decodePoint(Coll[Int](1))")
  }

  property("xorOf") {
    typecheck(env, "xorOf(Coll[Boolean](true, false))") shouldBe SBoolean
    an[TyperException] should be thrownBy typecheck(env, "xorOf(Coll[Int](1))")
  }

  property("outerJoin") {
    typecheck(env,
      """outerJoin[Byte, Short, Int, Long](
        | Coll[(Byte, Short)]((1.toByte, 2.toShort)),
        | Coll[(Byte, Int)]((1.toByte, 3.toInt)),
        | { (b: Byte, s: Short) => (b + s).toLong },
        | { (b: Byte, i: Int) => (b + i).toLong },
        | { (b: Byte, s: Short, i: Int) => (b + s + i).toLong }
        | )""".stripMargin) shouldBe SCollection(STuple(SByte, SLong))
  }

  property("AtLeast (invalid parameters)") {
    an [TyperException] should be thrownBy typecheck(env, "atLeast(2, 2)")
  }

  property("substConstants") {
    typecheck(env, "substConstants[Long](Coll[Byte](1.toByte), Coll[Int](1), Coll[Long](1L))") shouldBe SByteArray
  }

  property("executeFromVar") {
    typecheck(env, "executeFromVar[Boolean](1)") shouldBe SBoolean
  }

  property("LogicalNot") {
    typecheck(env, "!true") shouldBe SBoolean
    an [TyperException] should be thrownBy typecheck(env, "!getVar[SigmaProp](1).get")
  }

  property("Negation") {
    typecheck(env, "-HEIGHT") shouldBe SInt
    an [TyperException] should be thrownBy typecheck(env, "-true")
  }

  property("BitInversion") {
    typecheck(env, "~1") shouldBe SInt
    an [TyperException] should be thrownBy typecheck(env, "~true")
  }

  property("LogicalXor") {
    typecheck(env, "true ^ false") shouldBe SBoolean
  }

  property("BitwiseOr") {
    typecheck(env, "1 | 2") shouldBe SInt
    an [TyperException] should be thrownBy typecheck(env, "true | false")
  }

  property("BitwiseAnd") {
    typecheck(env, "1 & 2") shouldBe SInt
    an [TyperException] should be thrownBy typecheck(env, "true & false")
  }

  property("BitwiseXor") {
    typecheck(env, "1 ^ 2") shouldBe SInt
  }

  property("BitShiftRight") {
    typecheck(env, "1 >> 2") shouldBe SInt
    an [TyperException] should be thrownBy typecheck(env, "true >> false")
  }

  property("BitShiftLeft") {
    typecheck(env, "1 << 2") shouldBe SInt
    an [TyperException] should be thrownBy typecheck(env, "true << false")
  }

  property("BitShiftRightZeroed") {
    typecheck(env, "1 >>> 2") shouldBe SInt
    an [TyperException] should be thrownBy typecheck(env, "true >>> false")
  }

}
