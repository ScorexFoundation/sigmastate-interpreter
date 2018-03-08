package sigmastate.lang

import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.utxo.SizeOf

class SigmaParserTest extends PropSpec with PropertyChecks with Matchers {

  def parse(x: String): SValue = {
    try {
      val res = SigmaParser(x).get.value
//      Parser.logged.foreach(println)
      res
    } catch {
      case e: Exception =>
        SigmaParser.logged.foreach(println)
        throw e
    }
  }

  property("simple expressions") {
    parse("10") shouldBe IntConstant(10)
    parse("10+11") shouldBe Plus(IntConstant(10), IntConstant(11))
    parse("(10+11)") shouldBe Plus(IntConstant(10), IntConstant(11))
    parse("(10+11) + 12") shouldBe Plus(Plus(IntConstant(10), IntConstant(11)), IntConstant(12))
    parse("10   + 11 + 12") shouldBe Plus(Plus(IntConstant(10), IntConstant(11)), IntConstant(12))
    parse("1+2+3+4+5") shouldBe Plus(Plus(Plus(Plus(IntConstant(1), IntConstant(2)), IntConstant(3)), IntConstant(4)), IntConstant(5))
    parse("1==1") shouldBe EQ(IntConstant(1), IntConstant(1))
    parse("true && true") shouldBe AND(TrueLeaf, TrueLeaf)
    parse("true || false") shouldBe OR(TrueLeaf, FalseLeaf)
    parse("true || (true && false)") shouldBe OR(TrueLeaf, AND(TrueLeaf, FalseLeaf))
    parse("false || false || false") shouldBe OR(OR(FalseLeaf, FalseLeaf), FalseLeaf)
    parse("(1>= 0)||(3 >2)") shouldBe OR(GE(IntConstant(1), IntConstant(0)), GT(IntConstant(3), IntConstant(2)))
  }

  property("priority in binary expressions") {
    parse("1 + 0 + 3") shouldBe Plus(Plus(IntConstant(1), IntConstant(0)), IntConstant(3))
    parse("1 == 0 || 3 == 2") shouldBe OR(EQ(IntConstant(1), IntConstant(0)), EQ(IntConstant(3), IntConstant(2)))
    parse("3 + 2 > 2 + 1") shouldBe GT(Plus(IntConstant(3), IntConstant(2)), Plus(IntConstant(2), IntConstant(1)))
    parse("1 >= 0 || 3 > 2") shouldBe OR(GE(IntConstant(1), IntConstant(0)), GT(IntConstant(3), IntConstant(2)))
  }

  property("tuple constructor") {
    parse("()") shouldBe UnitConstant
    parse("(1)") shouldBe IntConstant(1)
    parse("(1, 2)") shouldBe Tuple(IntConstant(1), IntConstant(2))
    parse("(1, X + 1)") shouldBe Tuple(IntConstant(1), Plus(Ident("X").asValue[SInt.type], IntConstant(1)))
    parse("(1, 2, 3)") shouldBe Tuple(IntConstant(1), IntConstant(2), IntConstant(3))
    parse("(1, 2 + 3, 4)") shouldBe Tuple(IntConstant(1), Plus(IntConstant(2), IntConstant(3)), IntConstant(4))
  }

  property("let constructs") {
    parse(
      """{let X = 10
        |3 > 2}
      """.stripMargin) shouldBe Block(Some(Let("X", None, IntConstant(10))), GT(IntConstant(3), IntConstant(2)))

    parse("{let X = 10; 3 > 2}") shouldBe Block(Some(Let("X", None, IntConstant(10))), GT(IntConstant(3), IntConstant(2)))
    parse("{let X = 3 + 2; 3 > 2}") shouldBe Block(Some(Let("X", None, Plus(IntConstant(3), IntConstant(2)))), GT(IntConstant(3), IntConstant(2)))
    parse("{let X = if (true) true else false; false}") shouldBe Block(Some(Let("X", None, If(TrueLeaf, TrueLeaf, FalseLeaf))), FalseLeaf)

    val expr = parse(
      """{let X = 10
        |let Y = 11
        |X > Y}
      """.stripMargin)

    expr shouldBe Block(Some(Let("X", None, IntConstant(10))), Block(Some(Let("Y", None, IntConstant(11))), typed[SInt.type, SInt.type](Ident("X"), Ident("Y"))(GT)))
  }

  property("types") {
    parse("{let X: Int = 10; 3 > 2}") shouldBe Block(Some(Let("X", SInt, IntConstant(10))), GT(IntConstant(3), IntConstant(2)))
    parse("""{let X: (Int, Boolean) = (10, true); 3 > 2}""") shouldBe Block(Some(Let("X", STuple(SInt, SBoolean), Tuple(IntConstant(10), TrueLeaf))), GT(IntConstant(3), IntConstant(2)))
    parse("""{let X: Array[Int] = Array(1,2,3); X.size}""") shouldBe
      Block(Some(Let("X", SCollection(SInt), ConcreteCollection(IndexedSeq(IntConstant(1), IntConstant(2), IntConstant(3))))),
            Select(Ident("X"), "size"))
    parse("""{let X: (Array[Int], Box) = (Array(1,2,3), INPUT); X._1}""") shouldBe
        Block(Some(Let("X", STuple(SCollection(SInt), SBox), Tuple(ConcreteCollection(IndexedSeq(IntConstant(1), IntConstant(2), IntConstant(3))), Ident("INPUT")))),
          Select(Ident("X"), "_1"))
  }

  property("multiline") {
    parse(
      """
        |
        |false
        |
        |
      """.stripMargin) shouldBe FalseLeaf

    parse(
      """{let X = 10;
        |
        |true}
      """.stripMargin) shouldBe Block(Some(Let("X", IntConstant(10))), TrueLeaf)
    parse(
      """{let X = 11
        |true}
      """.stripMargin) shouldBe Block(Some(Let("X", IntConstant(11))), TrueLeaf)
  }

  property("comments") {
    parse(
      """{
       |// line comment
       |let X = 12
       |/* comment // nested line comment
       |*/
       |3 + // end line comment
       |  2
       |}
      """.stripMargin) shouldBe Block(Some(Let("X", IntConstant(12))), Plus(IntConstant(3), IntConstant(2)))
  }

  property("if") {
    parse("if(true) 1 else 2") shouldBe If(TrueLeaf, IntConstant(1), IntConstant(2))
    parse("if(true) 1 else if(X==Y) 2 else 3") shouldBe If(TrueLeaf, IntConstant(1), If(EQ(Ident("X"), Ident("Y")), IntConstant(2), IntConstant(3)))
    parse(
      """if ( true )
        |1
        |else if(X== Y)
        |     2
        |     else 3""".stripMargin) shouldBe If(TrueLeaf, IntConstant(1), If(EQ(Ident("X"), Ident("Y")), IntConstant(2), IntConstant(3)))

    parse("if (true) false else false==false") shouldBe If(TrueLeaf, FalseLeaf, EQ(FalseLeaf, FalseLeaf))

    parse(
      """if

             (true)
        |{ let A = 10;
        |  1 }
        |else if ( X == Y) 2 else 3""".stripMargin) shouldBe If(
      TrueLeaf,
      Block(Some(Let("A", Block(None, IntConstant(10)))), IntConstant(1)),
      If(EQ(Ident("X"), Ident("Y")), IntConstant(2), IntConstant(3))
    )

  }

  property("array literals") {
    parse("Array()") shouldBe(ConcreteCollection(IndexedSeq.empty)(NoType))
    parse("Array(1)") shouldBe(ConcreteCollection(IndexedSeq(IntConstant(1)))(SInt))
    parse("Array(1, X)") shouldBe(ConcreteCollection(IndexedSeq(IntConstant(1), Ident("X")))(SInt))
    parse("Array(1, X + 1, Array())") shouldBe(ConcreteCollection(
      IndexedSeq(
        IntConstant(1),
        Plus(Ident("X").asValue[SInt.type], IntConstant(1)),
        ConcreteCollection(IndexedSeq.empty)(NoType)))(SInt))
    parse("Array(Array(X + 1))") shouldBe ConcreteCollection[SCollection[SInt.type]](
      IndexedSeq(ConcreteCollection[SInt.type](IndexedSeq(
                  Plus(Ident("X").asValue[SInt.type], IntConstant(1))))))
  }

  property("array indexed access") {
    parse("Array()(0)") shouldBe Apply(ConcreteCollection(IndexedSeq.empty)(NoType), IndexedSeq(IntConstant(0)))
  }

  property("global functions") {
    parse("f(x)") shouldBe Apply(Ident("f"), IndexedSeq(Ident("x")))
    parse("f((x, y))") shouldBe Apply(Ident("f"), IndexedSeq(Tuple(IndexedSeq(Ident("x"), Ident("y")))))
    parse("f(x, y)") shouldBe Apply(Ident("f"), IndexedSeq(Ident("x"), Ident("y")))
    parse("f(x, y).size") shouldBe Select(Apply(Ident("f"), IndexedSeq(Ident("x"), Ident("y"))), "size")
    parse("f(x, y).get(1)") shouldBe Apply(Select(Apply(Ident("f"), IndexedSeq(Ident("x"), Ident("y"))), "get"), IndexedSeq(IntConstant(1)))
    parse("{let y = f(x); y}") shouldBe Block(Some(Let("y", Apply(Ident("f"), IndexedSeq(Ident("x"))))), Ident("y"))
  }

  property("lambdas") {
    parse("fun (x: Int) = x + 1") shouldBe
      Lambda(IndexedSeq("x" -> SInt), Plus(Ident("x").asValue[SInt.type], IntConstant(1)))
    parse("fun (x: Int): Int = x + 1") shouldBe
      Lambda(IndexedSeq("x" -> SInt), SInt, Plus(Ident("x").asValue[SInt.type], IntConstant(1)))
    parse("fun (x: Int, box: Box): Int = x + box.value") shouldBe
        Lambda(IndexedSeq("x" -> SInt, "box" -> SBox), SInt,
               Plus(Ident("x").asValue[SInt.type], Select(Ident("box"), "value").asValue[SInt.type]))
    parse("fun (p: (Int, GroupElement), box: Box): Int = p._1 > box.value && p._2.isIdentity") shouldBe
        Lambda(IndexedSeq("p" -> STuple(SInt, SGroupElement), "box" -> SBox), SInt,
          AND(
            GT(Select(Ident("p"), "_1").asValue[SInt.type], Select(Ident("box"), "value").asValue[SInt.type]),
            Select(Select(Ident("p"), "_2"), "isIdentity").asValue[SBoolean.type]
            )
        )

    parse("fun (x) = x + 1") shouldBe
        Lambda(IndexedSeq("x" -> NoType), None, Plus(Ident("x").asValue[SInt.type], IntConstant(1)))
    parse("fun (x: Int) = { x + 1 }") shouldBe
        Lambda(IndexedSeq("x" -> SInt), None, Plus(Ident("x").asValue[SInt.type], IntConstant(1)))
    parse("fun (x: Int) = { let y = x + 1; y }") shouldBe
        Lambda(IndexedSeq("x" -> SInt), None,
          Block(Let("y", Plus(Ident("x").asValue[SInt.type], IntConstant(1))), Ident("y")))
  }

  property("unary operations") {
    parse("!x") shouldBe Not(Ident("x").asValue[SBoolean.type])
    parse("!x && y") shouldBe AND(Not(Ident("x").asValue[SBoolean.type]), Ident("y").asValue[SBoolean.type])
    parse("!x && !y") shouldBe AND(Not(Ident("x").asValue[SBoolean.type]), Not(Ident("y").asValue[SBoolean.type]))
  }

  property("get field of ref") {
    parse("XXX.YYY") shouldBe Select(Ident("XXX"), "YYY")
    parse("""
        |
        | X.Y
        |
      """.stripMargin) shouldBe Select(Ident("X"), "Y")
  }

}
