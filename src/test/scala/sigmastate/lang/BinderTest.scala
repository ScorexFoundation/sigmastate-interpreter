package sigmastate.lang

import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate.utxo._

class BinderTest extends PropSpec with PropertyChecks with Matchers {

  def bind(env: Map[String, Any], x: String): SValue = {
    try {
      val ast = SigmaParser(x).get.value
      val binder = new SigmaBinder(env)
      binder.bind(ast)
    } catch {
      case e: Exception =>
        SigmaParser.logged.foreach(println)
        throw e
    }
  }

  def BoolIdent(name: String): Value[SBoolean.type] = Ident(name).asValue[SBoolean.type]
  def IntIdent(name: String): Value[SInt.type] = Ident(name).asValue[SInt.type]

  val EV: Map[String, Any] = Map()

  val env = Map("x" -> 10, "y" -> 11, "c1" -> true, "c2" -> false, "arr" -> Array[Byte](1, 2))

  property("simple expressions") {
    bind(env, "x") shouldBe IntConstant(10)
    bind(env, "x+y") shouldBe Plus(10, 11)
    bind(env, "c1 && c2") shouldBe AND(TrueLeaf, FalseLeaf)
    bind(env, "arr") shouldBe ByteArrayConstant(Array(1, 2))
    bind(env, "HEIGHT + 1") shouldBe Plus(Height, 1)
    bind(env, "INPUTS.size > 1") shouldBe GT(SizeOf(Inputs), 1)
  }

  property("predefined functions") {
    bind(env, "all(Array(c1, c2))") shouldBe
        AND(ConcreteCollection(Vector(TrueLeaf, FalseLeaf)))
  }

    property("let constructs") {
      bind(env, "{let X = 10; X > 2}") shouldBe
        Block(Let("X", SInt, IntConstant(10)), GT(IntIdent("X"), 2))
      bind(env, "{let X = 10; X >= X}") shouldBe
        Block(Let("X", SInt, IntConstant(10)), GE(IntIdent("X"), IntIdent("X")))
      bind(env, "{let X = 10 + 1; X >= X}") shouldBe
          Block(Let("X", SInt, Plus(10, 1)), GE(IntIdent("X"), IntIdent("X")))
      bind(env,
        """{let X = 10
         |let Y = 11
         |X > Y}
        """.stripMargin) shouldBe Block(
        Seq(Let("X", SInt, IntConstant(10)), Let("Y", SInt, IntConstant(11))),
        GT(IntIdent("X"), IntIdent("Y")))
    }


//  property("predefined Exists with lambda argument") {
//    val minToRaise = IntConstant(1000)
//    val env = this.env ++ Map(
//      "minToRaise" -> minToRaise,
//    )
//    bind(env, """exists(OUTPUTS, fun (out: Box) = { out.amount >= minToRaise })""") shouldBe
//        Exists(Outputs, 21, GE(ExtractAmount(TaggedBox(21)), minToRaise))
//  }

  property("tuple constructor") {
    bind(env, "()") shouldBe UnitConstant
    bind(env, "(1)") shouldBe IntConstant(1)
    bind(env, "(1, 2)") shouldBe Tuple(IntConstant(1), IntConstant(2))
    bind(env, "(1, x + 1)") shouldBe Tuple(IntConstant(1), Plus(10, 1))
    bind(env, "(1, 2, 3)") shouldBe Tuple(IntConstant(1), IntConstant(2), IntConstant(3))
    bind(env, "(1, 2 + 3, 4)") shouldBe Tuple(IntConstant(1), Plus(2, 3), IntConstant(4))
  }

  property("types") {
    bind(env, "{let X: Int = 10; 3 > 2}") shouldBe Block(Let("X", SInt, IntConstant(10)), GT(3, 2))
    bind(env, "{let X: (Int, Boolean) = (10, true); 3 > 2}") shouldBe
      Block(Let("X", STuple(SInt, SBoolean), Tuple(IntConstant(10), TrueLeaf)), GT(3, 2))
    bind(env, "{let X: Array[Int] = Array(1,2,3); X.size}") shouldBe
      Block(Let("X", SCollection(SInt), ConcreteCollection(IndexedSeq(IntConstant(1), IntConstant(2), IntConstant(3)))),
            Select(Ident("X"), "size"))
    bind(env, "{let X: (Array[Int], Box) = (Array(1,2,3), INPUT); X._1}") shouldBe
      Block(Let("X", STuple(SCollection(SInt), SBox), Tuple(ConcreteCollection(IndexedSeq(IntConstant(1), IntConstant(2), IntConstant(3))), Ident("INPUT"))),
            Select(Ident("X"), "_1"))
  }

  property("if") {
    bind(env, "if (true) x else y") shouldBe If(TrueLeaf, IntConstant(10), IntConstant(11))
    bind(env, "if(c1) 1 else if(x==y) 2 else 3") shouldBe
      If(TrueLeaf, IntConstant(1), If(EQ(IntConstant(10), IntConstant(11)), IntConstant(2), IntConstant(3)))
    bind(env,
      """if (true) { let A = x; 1 }
        |else if (x == y) 2 else 3""".stripMargin) shouldBe
        If(TrueLeaf,
          Block(Let("A", SInt, IntConstant(10)), IntConstant(1)),
          If(EQ(IntConstant(10), IntConstant(11)), IntConstant(2), IntConstant(3)))
  }

  property("array indexed access") {
    bind(env, "Array()(0)") shouldBe Apply(ConcreteCollection(IndexedSeq.empty)(NoType), IndexedSeq(IntConstant(0)))
    bind(env, "Array()(0)(0)") shouldBe Apply(Apply(ConcreteCollection(IndexedSeq.empty)(NoType), IndexedSeq(IntConstant(0))), IndexedSeq(IntConstant(0)))
  }

  property("lambdas") {
    bind(env, "fun (a: Int) = a + 1") shouldBe
      Lambda(IndexedSeq("a" -> SInt), SInt, Plus(IntIdent("a"), 1))
    bind(env, "fun (a: Int, box: Box): Int = a + box.value") shouldBe
        Lambda(IndexedSeq("a" -> SInt, "box" -> SBox), SInt,
               Plus(IntIdent("a"), Select(Ident("box"), "value").asValue[SInt.type]))
    bind(env, "fun (a) = a + 1") shouldBe
        Lambda(IndexedSeq("a" -> NoType), SInt, Plus(IntIdent("a"), IntConstant(1)))
    bind(env, "fun (a) = a + x") shouldBe
        Lambda(IndexedSeq("a" -> NoType), SInt, Plus(IntIdent("a"), 10))
    bind(env, "fun (a: Int) = { let Y = a + 1; Y + x }") shouldBe
        Lambda(IndexedSeq("a" -> SInt), SInt,
          Block(Let("Y", SInt, Plus(IntIdent("a"), 1)), Plus(IntIdent("Y"), 10)))
  }

//  property("function definitions") {
//    bind(
//      """{let f = fun (x: Int) = x + 1
//       |f}
//      """.stripMargin) shouldBe
//        Block(Let("f", Lambda(IndexedSeq("x" -> SInt), Plus(IntIdent("x"), 1))), Ident("f"))
//    bind(
//      """{fun f(x: Int) = x + 1
//       |f}
//      """.stripMargin) shouldBe
//        Block(Let("f", Lambda(IndexedSeq("x" -> SInt), Plus(IntIdent("x"), 1))), Ident("f"))
//  }
//
  property("unary operations") {
    bind(env, "!c1") shouldBe Not(TrueLeaf)
    bind(env, "!c1 && c2") shouldBe AND(Not(TrueLeaf), FalseLeaf)
    bind(env, "!c1 && !c2") shouldBe AND(Not(TrueLeaf), Not(FalseLeaf))
  }
}
