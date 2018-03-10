package sigmastate.lang

import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.utxo.{Height, SizeOf, Inputs}

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
//    bind("f((x, y))") shouldBe Apply(Ident("f"), IndexedSeq(Tuple(IndexedSeq(Ident("x"), Ident("y")))))
//    bind("f(x, y)") shouldBe Apply(Ident("f"), IndexedSeq(Ident("x"), Ident("y")))
//    bind("f(x, y).size") shouldBe Select(Apply(Ident("f"), IndexedSeq(Ident("x"), Ident("y"))), "size")
//    bind("f(x, y).get(1)") shouldBe Apply(Select(Apply(Ident("f"), IndexedSeq(Ident("x"), Ident("y"))), "get"), IndexedSeq(IntConstant(1)))
//    bind("{let y = f(x); y}") shouldBe Block(Some(Let("y", Apply(Ident("f"), IndexedSeq(Ident("x"))))), Ident("y"))
  }

//  property("tuple constructor") {
//    bind("()") shouldBe UnitConstant
//    bind("(1)") shouldBe IntConstant(1)
//    bind("(1, 2)") shouldBe Tuple(IntConstant(1), IntConstant(2))
//    bind("(1, X + 1)") shouldBe Tuple(IntConstant(1), Plus(IntIdent("X"), 1))
//    bind("(1, 2, 3)") shouldBe Tuple(IntConstant(1), IntConstant(2), IntConstant(3))
//    bind("(1, 2 + 3, 4)") shouldBe Tuple(IntConstant(1), Plus(2, 3), IntConstant(4))
//  }
//
//  property("let constructs") {
//    bind(
//      """{let X = 10
//        |3 > 2}
//      """.stripMargin) shouldBe Block(Some(Let("X", None, IntConstant(10))), GT(3, 2))
//
//    bind("{let X = 10; 3 > 2}") shouldBe Block(Some(Let("X", None, IntConstant(10))), GT(3, 2))
//    bind("{let X = 3 + 2; 3 > 2}") shouldBe Block(Some(Let("X", None, Plus(3, 2))), GT(3, 2))
//    bind("{let X = if (true) true else false; false}") shouldBe Block(Some(Let("X", None, If(TrueLeaf, TrueLeaf, FalseLeaf))), FalseLeaf)
//
//    val expr = bind(
//      """{let X = 10
//        |let Y = 11
//        |X > Y}
//      """.stripMargin)
//
//    expr shouldBe Block(Some(Let("X", None, IntConstant(10))), Block(Some(Let("Y", None, IntConstant(11))), GT(IntIdent("X"), IntIdent("Y"))))
//  }
//
//  property("types") {
//    bind("{let X: Int = 10; 3 > 2}") shouldBe Block(Some(Let("X", SInt, IntConstant(10))), GT(3, 2))
//    bind("""{let X: (Int, Boolean) = (10, true); 3 > 2}""") shouldBe
//      Block(Some(Let("X", STuple(SInt, SBoolean), Tuple(IntConstant(10), TrueLeaf))), GT(3, 2))
//    bind("""{let X: Array[Int] = Array(1,2,3); X.size}""") shouldBe
//      Block(Some(Let("X", SCollection(SInt), ConcreteCollection(IndexedSeq(IntConstant(1), IntConstant(2), IntConstant(3))))),
//            Select(Ident("X"), "size"))
//    bind("""{let X: (Array[Int], Box) = (Array(1,2,3), INPUT); X._1}""") shouldBe
//        Block(Some(Let("X", STuple(SCollection(SInt), SBox), Tuple(ConcreteCollection(IndexedSeq(IntConstant(1), IntConstant(2), IntConstant(3))), Ident("INPUT")))),
//          Select(Ident("X"), "_1"))
//  }
//
//  property("multiline") {
//    bind(
//      """
//        |
//        |false
//        |
//        |
//      """.stripMargin) shouldBe FalseLeaf
//
//    bind(
//      """{let X = 10;
//        |
//        |true}
//      """.stripMargin) shouldBe Block(Some(Let("X", IntConstant(10))), TrueLeaf)
//    bind(
//      """{let X = 11
//        |true}
//      """.stripMargin) shouldBe Block(Some(Let("X", IntConstant(11))), TrueLeaf)
//  }
//
//  property("comments") {
//    bind(
//      """{
//       |// line comment
//       |let X = 12
//       |/* comment // nested line comment
//       |*/
//       |3 + // end line comment
//       |  2
//       |}
//      """.stripMargin) shouldBe Block(Some(Let("X", IntConstant(12))), Plus(3, 2))
//  }
//
//  property("if") {
//    bind("if(true) 1 else 2") shouldBe If(TrueLeaf, IntConstant(1), IntConstant(2))
//    bind("if(true) 1 else if(X==Y) 2 else 3") shouldBe If(TrueLeaf, IntConstant(1), If(EQ(Ident("X"), Ident("Y")), IntConstant(2), IntConstant(3)))
//    bind(
//      """if ( true )
//        |1
//        |else if(X== Y)
//        |     2
//        |     else 3""".stripMargin) shouldBe If(TrueLeaf, IntConstant(1), If(EQ(Ident("X"), Ident("Y")), IntConstant(2), IntConstant(3)))
//
//    bind("if (true) false else false==false") shouldBe If(TrueLeaf, FalseLeaf, EQ(FalseLeaf, FalseLeaf))
//
//    bind(
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
//  property("array literals") {
//    val emptyCol = ConcreteCollection(IndexedSeq.empty)(NoType)
//    bind("Array()") shouldBe(emptyCol)
//    val emptyCol2 = ConcreteCollection(IndexedSeq(emptyCol))(SCollection(NoType))
//    bind("Array(Array())") shouldBe(emptyCol2)
//    bind("Array(Array(Array()))") shouldBe(ConcreteCollection(IndexedSeq(emptyCol2))(SCollection(SCollection(NoType))))
//
//    bind("Array(1)") shouldBe(ConcreteCollection(IndexedSeq(IntConstant(1)))(SInt))
//    bind("Array(1, X)") shouldBe(ConcreteCollection(IndexedSeq(IntConstant(1), Ident("X")))(SInt))
//    bind("Array(1, X + 1, Array())") shouldBe(ConcreteCollection(
//      IndexedSeq(
//        IntConstant(1),
//        Plus(Ident("X").asValue[SInt.type], IntConstant(1)),
//        ConcreteCollection(IndexedSeq.empty)(NoType)))(SInt))
//    bind("Array(Array(X + 1))") shouldBe ConcreteCollection[SCollection[SInt.type]](
//      IndexedSeq(ConcreteCollection[SInt.type](IndexedSeq(
//                  Plus(Ident("X").asValue[SInt.type], IntConstant(1))))))
//  }
//
//  property("array indexed access") {
//    bind("Array()(0)") shouldBe Apply(ConcreteCollection(IndexedSeq.empty)(NoType), IndexedSeq(IntConstant(0)))
//    bind("Array()(0)(0)") shouldBe Apply(Apply(ConcreteCollection(IndexedSeq.empty)(NoType), IndexedSeq(IntConstant(0))), IndexedSeq(IntConstant(0)))
//  }

//  property("lambdas") {
//    bind("fun (x: Int) = x + 1") shouldBe
//      Lambda(IndexedSeq("x" -> SInt), Plus(Ident("x").asValue[SInt.type], IntConstant(1)))
//    bind("fun (x: Int): Int = x + 1") shouldBe
//      Lambda(IndexedSeq("x" -> SInt), SInt, Plus(Ident("x").asValue[SInt.type], IntConstant(1)))
//    bind("fun (x: Int, box: Box): Int = x + box.value") shouldBe
//        Lambda(IndexedSeq("x" -> SInt, "box" -> SBox), SInt,
//               Plus(Ident("x").asValue[SInt.type], Select(Ident("box"), "value").asValue[SInt.type]))
//    bind("fun (p: (Int, GroupElement), box: Box): Int = p._1 > box.value && p._2.isIdentity") shouldBe
//        Lambda(IndexedSeq("p" -> STuple(SInt, SGroupElement), "box" -> SBox), SInt,
//          AND(
//            GT(Select(Ident("p"), "_1").asValue[SInt.type], Select(Ident("box"), "value").asValue[SInt.type]),
//            Select(Select(Ident("p"), "_2"), "isIdentity").asValue[SBoolean.type]
//            )
//        )
//
//    bind("fun (x) = x + 1") shouldBe
//        Lambda(IndexedSeq("x" -> NoType), Plus(Ident("x").asValue[SInt.type], IntConstant(1)))
//    bind("fun (x: Int) = { x + 1 }") shouldBe
//        Lambda(IndexedSeq("x" -> SInt), Plus(Ident("x").asValue[SInt.type], IntConstant(1)))
//    bind("fun (x: Int) = { let y = x + 1; y }") shouldBe
//        Lambda(IndexedSeq("x" -> SInt),
//          Block(Let("y", Plus(IntIdent("x"), 1)), Ident("y")))
//  }
//
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
//  property("unary operations") {
//    bind("!x") shouldBe Not(Ident("x").asValue[SBoolean.type])
//    bind("!x && y") shouldBe AND(Not(Ident("x").asValue[SBoolean.type]), Ident("y").asValue[SBoolean.type])
//    bind("!x && !y") shouldBe AND(Not(Ident("x").asValue[SBoolean.type]), Not(Ident("y").asValue[SBoolean.type]))
//  }
}
