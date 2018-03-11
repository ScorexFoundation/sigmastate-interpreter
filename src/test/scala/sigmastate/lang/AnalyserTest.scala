package sigmastate.lang

import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate.lang.SigmaPredef._

class AnalyserTest extends PropSpec with PropertyChecks with Matchers {

  def typecheck(env: Map[String, Any], x: String): SType = {
    try {
      val parsed = SigmaParser(x).get.value
      val binder = new SigmaBinder(env)
      val bound = binder.bind(parsed)
      val st = new SigmaTree(bound)
      val an = new Analyser(env, st)
      an.errors shouldBe empty
      an.tipe(bound)
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
//    typecheck(env, "all(Array(c1, c2))") shouldBe
//        AND(ConcreteCollection(Vector(TrueLeaf, FalseLeaf)))
  }
//
//    property("let constructs") {
//      typecheck(env, """{let X = 10; X > 2}""".stripMargin) shouldBe
//        Block(Let("X", None, IntConstant(10)), GT(10, 2))
//      typecheck(env, """{let X = 10; X >= X}""".stripMargin) shouldBe
//        Block(Let("X", None, IntConstant(10)), GE(10, 10))
//      typecheck(env, """{let X = 10 + 1; X >= X}""".stripMargin) shouldBe
//          Block(Let("X", None, Plus(10, 1)), GE(Plus(10, 1), Plus(10, 1)))
//      typecheck(env,
//        """{let X = 10
//         |let Y = 11
//         |X > Y}
//        """.stripMargin) shouldBe Block(
//        Seq(Let("X", None, IntConstant(10)), Let("Y", None, IntConstant(11))),
//        GT(10, 11))
//    }
//
//
//  property("predefined Exists with lambda argument") {
//    val minToRaise = IntConstant(1000)
//    val env = this.env ++ Map(
//      "minToRaise" -> minToRaise,
//    )
//    typecheck(env, """exists(OUTPUTS, fun (out: Box) = { out.amount >= minToRaise })""") shouldBe
//        Exists(Outputs, 21, GE(ExtractAmount(TaggedBox(21)), minToRaise))
//  }
//
//  property("tuple constructor") {
//    typecheck("()") shouldBe UnitConstant
//    typecheck("(1)") shouldBe IntConstant(1)
//    typecheck("(1, 2)") shouldBe Tuple(IntConstant(1), IntConstant(2))
//    typecheck("(1, X + 1)") shouldBe Tuple(IntConstant(1), Plus(IntIdent("X"), 1))
//    typecheck("(1, 2, 3)") shouldBe Tuple(IntConstant(1), IntConstant(2), IntConstant(3))
//    typecheck("(1, 2 + 3, 4)") shouldBe Tuple(IntConstant(1), Plus(2, 3), IntConstant(4))
//  }
//
//  property("types") {
//    typecheck("{let X: Int = 10; 3 > 2}") shouldBe Block(Some(Let("X", SInt, IntConstant(10))), GT(3, 2))
//    typecheck("""{let X: (Int, Boolean) = (10, true); 3 > 2}""") shouldBe
//      Block(Some(Let("X", STuple(SInt, SBoolean), Tuple(IntConstant(10), TrueLeaf))), GT(3, 2))
//    typecheck("""{let X: Array[Int] = Array(1,2,3); X.size}""") shouldBe
//      Block(Some(Let("X", SCollection(SInt), ConcreteCollection(IndexedSeq(IntConstant(1), IntConstant(2), IntConstant(3))))),
//            Select(Ident("X"), "size"))
//    typecheck("""{let X: (Array[Int], Box) = (Array(1,2,3), INPUT); X._1}""") shouldBe
//        Block(Some(Let("X", STuple(SCollection(SInt), SBox), Tuple(ConcreteCollection(IndexedSeq(IntConstant(1), IntConstant(2), IntConstant(3))), Ident("INPUT")))),
//          Select(Ident("X"), "_1"))
//  }
//
//  property("multiline") {
//    typecheck(
//      """
//        |
//        |false
//        |
//        |
//      """.stripMargin) shouldBe FalseLeaf
//
//    typecheck(
//      """{let X = 10;
//        |
//        |true}
//      """.stripMargin) shouldBe Block(Some(Let("X", IntConstant(10))), TrueLeaf)
//    typecheck(
//      """{let X = 11
//        |true}
//      """.stripMargin) shouldBe Block(Some(Let("X", IntConstant(11))), TrueLeaf)
//  }
//
//  property("comments") {
//    typecheck(
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
//    typecheck("if(true) 1 else 2") shouldBe If(TrueLeaf, IntConstant(1), IntConstant(2))
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
    val emptyCol = ConcreteCollection(IndexedSeq.empty)(NoType)
    typecheck(env, "Array()") shouldBe emptyCol.tpe
//    val emptyCol2 = ConcreteCollection(IndexedSeq(emptyCol))(SCollection(NoType))
//    typecheck("Array(Array())") shouldBe(emptyCol2)
//    typecheck("Array(Array(Array()))") shouldBe(ConcreteCollection(IndexedSeq(emptyCol2))(SCollection(SCollection(NoType))))
//
//    typecheck("Array(1)") shouldBe(ConcreteCollection(IndexedSeq(IntConstant(1)))(SInt))
//    typecheck("Array(1, X)") shouldBe(ConcreteCollection(IndexedSeq(IntConstant(1), Ident("X")))(SInt))
//    typecheck("Array(1, X + 1, Array())") shouldBe(ConcreteCollection(
//      IndexedSeq(
//        IntConstant(1),
//        Plus(Ident("X").asValue[SInt.type], IntConstant(1)),
//        ConcreteCollection(IndexedSeq.empty)(NoType)))(SInt))
//    typecheck("Array(Array(X + 1))") shouldBe ConcreteCollection[SCollection[SInt.type]](
//      IndexedSeq(ConcreteCollection[SInt.type](IndexedSeq(
//                  Plus(Ident("X").asValue[SInt.type], IntConstant(1))))))
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
