package sigmastate.lang

import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate.lang.SigmaPredef._

class SigmaTyperTest extends PropSpec with PropertyChecks with Matchers {

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
        SigmaParser.logged.foreach(println)
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
      assert(false, s"Should not typecheck: $x")
    } catch {
      case e: TyperException =>
        if (messageSubstr.nonEmpty)
          e.getMessage.indexOf(messageSubstr) should not be(-1)
    }
  }

  def BoolIdent(name: String): Value[SBoolean.type] = Ident(name).asValue[SBoolean.type]
  def IntIdent(name: String): Value[SInt.type] = Ident(name).asValue[SInt.type]

  val EV: Map[String, Any] = Map()

  val env = Map("x" -> 10, "y" -> 11, "c1" -> true, "c2" -> false, "arr" -> Array[Byte](1, 2))

  property("simple expressions") {
    typecheck(env, "x") shouldBe SInt
    typecheck(env, "x+y") shouldBe SInt
    typecheck(env, "c1 && c2") shouldBe SBoolean
    typecheck(env, "arr") shouldBe SByteArray
    typecheck(env, "HEIGHT") shouldBe SInt
    typecheck(env, "HEIGHT + 1") shouldBe SInt
    typecheck(env, "INPUTS") shouldBe SCollection(SBox)
    typecheck(env, "INPUTS.size") shouldBe SInt
    typecheck(env, "INPUTS.size > 1") shouldBe SBoolean
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

//  property("if") {
//    typecheck(env, "if(true) 1 else 2") shouldBe SInt
//    typecheck("if(true) 1 else if(X==Y) 2 else 3") shouldBe If(TrueLeaf, IntConstant(1), If(EQ(Ident("X"), Ident("Y")), IntConstant(2), IntConstant(3)))
//    typecheck(
//      """if ( true )
//        |1
//        |else if(X== Y)
//        |     2
//        |     else 3""".stripMargin) shouldBe If(TrueLeaf, IntConstant(1), If(EQ(Ident("X"), Ident("Y")), IntConstant(2), IntConstant(3)))
//
//    typecheck("if (true) false else false==false") shouldBe If(TrueLeaf, FalseLeaf, EQ(FalseLeaf, FalseLeaf))
//
//    typecheck(
//      """if
//
//             (true)
//        |{ let A = 10;
//        |  1 }
//        |else if ( X == Y) 2 else 3""".stripMargin) shouldBe If(
//      TrueLeaf,
//      Block(Some(Let("A", Block(None, IntConstant(10)))), IntConstant(1)),
//      If(EQ(Ident("X"), Ident("Y")), IntConstant(2), IntConstant(3))
//    )
//
//  }
//
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

//  property("array indexed access") {
//    typecheck("Array()(0)") shouldBe Apply(ConcreteCollection(IndexedSeq.empty)(NoType), IndexedSeq(IntConstant(0)))
//    typecheck("Array()(0)(0)") shouldBe Apply(Apply(ConcreteCollection(IndexedSeq.empty)(NoType), IndexedSeq(IntConstant(0))), IndexedSeq(IntConstant(0)))
//  }
//
//  property("lambdas") {
//    typecheck("fun (x: Int) = x + 1") shouldBe
//      Lambda(IndexedSeq("x" -> SInt), Plus(Ident("x").asValue[SInt.type], IntConstant(1)))
//    typecheck("fun (x: Int): Int = x + 1") shouldBe
//      Lambda(IndexedSeq("x" -> SInt), SInt, Plus(Ident("x").asValue[SInt.type], IntConstant(1)))
//    typecheck("fun (x: Int, box: Box): Int = x + box.value") shouldBe
//        Lambda(IndexedSeq("x" -> SInt, "box" -> SBox), SInt,
//               Plus(Ident("x").asValue[SInt.type], Select(Ident("box"), "value").asValue[SInt.type]))
//    typecheck("fun (p: (Int, GroupElement), box: Box): Int = p._1 > box.value && p._2.isIdentity") shouldBe
//        Lambda(IndexedSeq("p" -> STuple(SInt, SGroupElement), "box" -> SBox), SInt,
//          AND(
//            GT(Select(Ident("p"), "_1").asValue[SInt.type], Select(Ident("box"), "value").asValue[SInt.type]),
//            Select(Select(Ident("p"), "_2"), "isIdentity").asValue[SBoolean.type]
//            )
//        )
//
//    typecheck("fun (x) = x + 1") shouldBe
//        Lambda(IndexedSeq("x" -> NoType), Plus(Ident("x").asValue[SInt.type], IntConstant(1)))
//    typecheck("fun (x: Int) = { x + 1 }") shouldBe
//        Lambda(IndexedSeq("x" -> SInt), Plus(Ident("x").asValue[SInt.type], IntConstant(1)))
//    typecheck("fun (x: Int) = { let y = x + 1; y }") shouldBe
//        Lambda(IndexedSeq("x" -> SInt),
//          Block(Let("y", Plus(IntIdent("x"), 1)), Ident("y")))
//  }
//
//  property("function definitions") {
//    typecheck(
//      """{let f = fun (x: Int) = x + 1
//       |f}
//      """.stripMargin) shouldBe
//        Block(Let("f", Lambda(IndexedSeq("x" -> SInt), Plus(IntIdent("x"), 1))), Ident("f"))
//    typecheck(
//      """{fun f(x: Int) = x + 1
//       |f}
//      """.stripMargin) shouldBe
//        Block(Let("f", Lambda(IndexedSeq("x" -> SInt), Plus(IntIdent("x"), 1))), Ident("f"))
//  }
//
//  property("unary operations") {
//    typecheck("!x") shouldBe Not(Ident("x").asValue[SBoolean.type])
//    typecheck("!x && y") shouldBe AND(Not(Ident("x").asValue[SBoolean.type]), Ident("y").asValue[SBoolean.type])
//    typecheck("!x && !y") shouldBe AND(Not(Ident("x").asValue[SBoolean.type]), Not(Ident("y").asValue[SBoolean.type]))
//  }
}
