package sigmastate.lang

import java.beans.IndexedPropertyDescriptor

import fastparse.core.ParseError
import fastparse.core.Parsed.Failure
import org.scalactic.source.Position
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import sigmastate._
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate.utxo.{SizeOf, Exists}

class SigmaParserTest extends PropSpec with PropertyChecks with Matchers with LangTests {

  def parse(x: String): SValue = {
    try {
      val res = SigmaParser(x).get.value
      res
    } catch {
      case e: Exception =>
        throw e
    }
  }

  def fail(x: String, index: Int): Unit = {
    try {
      val res = SigmaParser(x).get.value
      assert(false, s"Error expected")
    } catch {
      case e: TestFailedException =>
        throw e
      case pe: ParseError[_,_] =>
        val l = pe.failure.index
        l shouldBe index
    }
  }

  property("simple expressions") {
    parse("10") shouldBe LongConstant(10)
    parse("10+11") shouldBe Plus(10, 11)
    parse("(10+11)") shouldBe Plus(10, 11)
    parse("(10+11) + 12") shouldBe Plus(Plus(10, 11), 12)
    parse("10   + 11 + 12") shouldBe Plus(Plus(10, 11), 12)
    parse("1+2+3+4+5") shouldBe Plus(Plus(Plus(Plus(1, 2), 3), 4), 5)
    parse("1==1") shouldBe EQ(1, 1)
    parse("true && true") shouldBe AND(TrueLeaf, TrueLeaf)
    parse("true || false") shouldBe OR(TrueLeaf, FalseLeaf)
    parse("true || (true && false)") shouldBe OR(TrueLeaf, AND(TrueLeaf, FalseLeaf))
    parse("false || false || false") shouldBe OR(OR(FalseLeaf, FalseLeaf), FalseLeaf)
    parse("(1>= 0)||(3 >2)") shouldBe OR(GE(1, 0), GT(3, 2))
    parse("arr1 | arr2") shouldBe Xor(ByteArrayIdent("arr1"), ByteArrayIdent("arr2"))
    parse("arr1 ++ arr2") shouldBe MethodCall(Ident("arr1"), "++", IndexedSeq(Ident("arr2")))
    parse("col1 ++ col2") shouldBe MethodCall(Ident("col1"), "++", IndexedSeq(Ident("col2")))
    parse("ge ^ n") shouldBe Exponentiate(GEIdent("ge"), BigIntIdent("n"))
    parse("g1 * g2") shouldBe MethodCall(Ident("g1"), "*", IndexedSeq(Ident("g2")))
  }

  property("precedence of binary operations") {
    parse("1 + 2 + 3") shouldBe Plus(Plus(1, 2), 3)
    parse("1 + 2 + 3 + 4") shouldBe Plus(Plus(Plus(1, 2), 3), 4)
    parse("1 == 0 || 3 == 2") shouldBe OR(EQ(1, 0), EQ(3, 2))
    parse("3 + 2 > 2 + 1") shouldBe GT(Plus(3, 2), Plus(2, 1))
    parse("1 + 2 + 3 > 4 + 5 + 6") shouldBe GT(Plus(Plus(1, 2), 3), Plus(Plus(4, 5), 6))
    parse("1 >= 0 || 3 > 2") shouldBe OR(GE(1, 0), GT(3, 2))
    parse("2 >= 0 + 1 || 3 - 1 >= 2") shouldBe OR(GE(2, Plus(0, 1)), GE(Minus(3, 1), 2))
    parse("x1 || x2 > x3 + x4 - x5 || x6") shouldBe
      OR(
        OR(BoolIdent("x1"),
           GT(IntIdent("x2"),
              Minus(Plus(IntIdent("x3"), IntIdent("x4")), IntIdent("x5")))),
        BoolIdent("x6"))
    parse("x1 || x2 > x3 + x4") shouldBe
      OR(BoolIdent("x1"),
        GT(IntIdent("x2"),
          Plus(IntIdent("x3"), IntIdent("x4"))))
  }

  property("tuple constructor") {
    parse("()") shouldBe UnitConstant
    parse("(1)") shouldBe LongConstant(1)
    parse("(1, 2)") shouldBe Tuple(LongConstant(1), LongConstant(2))
    parse("(1, X + 1)") shouldBe Tuple(LongConstant(1), Plus(IntIdent("X"), 1))
    parse("(1, 2, 3)") shouldBe Tuple(LongConstant(1), LongConstant(2), LongConstant(3))
    parse("(1, 2 + 3, 4)") shouldBe Tuple(LongConstant(1), Plus(2, 3), LongConstant(4))
  }

  property("let constructs") {
    parse(
      """{let X = 10
        |3 > 2}
      """.stripMargin) shouldBe Block(Let("X", LongConstant(10)), GT(3, 2))

    parse("{let X = 10; 3 > 2}") shouldBe Block(Let("X", LongConstant(10)), GT(3, 2))
    parse("{let X = 3 + 2; 3 > 2}") shouldBe Block(Let("X", Plus(3, 2)), GT(3, 2))
    parse("{let X = if (true) true else false; false}") shouldBe Block(Let("X", If(TrueLeaf, TrueLeaf, FalseLeaf)), FalseLeaf)

    val expr = parse(
      """{let X = 10
        |let Y = 11
        |X > Y}
      """.stripMargin)

    expr shouldBe Block(Seq(Let("X", LongConstant(10)),Let("Y", LongConstant(11))), GT(IntIdent("X"), IntIdent("Y")))
  }

  property("types") {
    parse("{let X: Byte = 10; 3 > 2}") shouldBe Block(Seq(Let("X", SByte, LongConstant(10))), GT(3, 2))
    parse("{let X: Int = 10; 3 > 2}") shouldBe Block(Seq(Let("X", SLong, LongConstant(10))), GT(3, 2))
    parse("""{let X: (Int, Boolean) = (10, true); 3 > 2}""") shouldBe
      Block(Seq(Let("X", STuple(SLong, SBoolean), Tuple(LongConstant(10), TrueLeaf))), GT(3, 2))
    parse("""{let X: Array[Int] = Array(1,2,3); X.size}""") shouldBe
      Block(Seq(Let("X", SCollection(SLong), Apply(Ident("Array"), IndexedSeq(LongConstant(1), LongConstant(2), LongConstant(3))))),
            Select(Ident("X"), "size"))
    parse("""{let X: (Array[Int], Box) = (Array(1,2,3), INPUT); X._1}""") shouldBe
        Block(Seq(Let("X", STuple(SCollection(SLong), SBox), Tuple(Apply(Ident("Array"), IndexedSeq(LongConstant(1), LongConstant(2), LongConstant(3))), Ident("INPUT")))),
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
      """.stripMargin) shouldBe Block(Seq(Let("X", LongConstant(10))), TrueLeaf)
    parse(
      """{let X = 11
        |true}
      """.stripMargin) shouldBe Block(Seq(Let("X", LongConstant(11))), TrueLeaf)
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
      """.stripMargin) shouldBe Block(Seq(Let("X", LongConstant(12))), Plus(3, 2))
  }

  property("if") {
    parse("if(true) 1 else 2") shouldBe If(TrueLeaf, LongConstant(1), LongConstant(2))
    parse("if(true) 1 else if(X==Y) 2 else 3") shouldBe If(TrueLeaf, LongConstant(1), If(EQ(Ident("X"), Ident("Y")), LongConstant(2), LongConstant(3)))
    parse(
      """if ( true )
        |1
        |else if(X== Y)
        |     2
        |     else 3""".stripMargin) shouldBe If(TrueLeaf, LongConstant(1), If(EQ(Ident("X"), Ident("Y")), LongConstant(2), LongConstant(3)))

    parse("if (true) false else false==false") shouldBe If(TrueLeaf, FalseLeaf, EQ(FalseLeaf, FalseLeaf))

    parse(
      """if

             (true)
        |{ let A = 10;
        |  1 }
        |else if ( X == Y) 2 else 3""".stripMargin) shouldBe
        If(TrueLeaf,
          Block(Seq(Let("A", LongConstant(10))), LongConstant(1)),
          If(EQ(Ident("X"), Ident("Y")), LongConstant(2), LongConstant(3))
    )

  }

  property("array literals") {
    val emptyCol = Apply(Ident("Array"), IndexedSeq.empty)
    parse("Array()") shouldBe emptyCol
    val emptyCol2 = Apply(Ident("Array"), IndexedSeq(emptyCol))
    parse("Array(Array())") shouldBe emptyCol2
    parse("Array(Array(Array()))") shouldBe Apply(Ident("Array"), IndexedSeq(emptyCol2))

    parse("Array(1)") shouldBe Apply(Ident("Array"), IndexedSeq(LongConstant(1)))
    parse("Array(1, X)") shouldBe Apply(Ident("Array"), IndexedSeq(LongConstant(1), Ident("X")))
    parse("Array(1, X + 1, Array())") shouldBe
    Apply(Ident("Array"),
      IndexedSeq(
        LongConstant(1),
        Plus(Ident("X").asValue[SLong.type], LongConstant(1)),
        Apply(Ident("Array"), IndexedSeq.empty)))
    parse("Array(Array(X + 1))") shouldBe Apply(Ident("Array"),
      IndexedSeq(Apply(Ident("Array"), IndexedSeq(
                  Plus(Ident("X").asValue[SLong.type], LongConstant(1))))))
  }

  property("Option constructors") {
    parse("None") shouldBe Ident("None")
    parse("Some(None)") shouldBe Apply(Ident("Some"), IndexedSeq(Ident("None")))
    parse("Some(10)") shouldBe Apply(Ident("Some"), IndexedSeq(LongConstant(10)))
    parse("Some(X)") shouldBe Apply(Ident("Some"), IndexedSeq(Ident("X")))
    parse("Some(Some(X + 1))") shouldBe Apply(Ident("Some"),
      IndexedSeq(Apply(Ident("Some"), IndexedSeq(
                  Plus(Ident("X").asValue[SLong.type], LongConstant(1))))))
  }

  property("array indexed access") {
    parse("Array()") shouldBe Apply(Ident("Array"), IndexedSeq.empty)
    parse("Array()(0)") shouldBe Apply(Apply(Ident("Array"), IndexedSeq.empty), IndexedSeq(LongConstant(0)))
    parse("Array()(0)(0)") shouldBe Apply(Apply(Apply(Ident("Array"), IndexedSeq.empty), IndexedSeq(LongConstant(0))), IndexedSeq(LongConstant(0)))
  }

  property("generic methods of arrays") {
    parse("OUTPUTS.map(fun (out: Box) = out.value)") shouldBe
      Apply(Select(Ident("OUTPUTS"), "map"),
            Vector(Lambda(Vector(("out",SBox)), Select(Ident("out"),"value"))))
    parse("OUTPUTS.exists(fun (out: Box) = out.value > 0)") shouldBe
      Apply(Select(Ident("OUTPUTS"), "exists"),
            Vector(Lambda(Vector(("out",SBox)), GT(Select(Ident("out"),"value").asIntValue, 0))))
    parse("OUTPUTS.forall(fun (out: Box) = out.value > 0)") shouldBe
      Apply(Select(Ident("OUTPUTS"), "forall"),
            Vector(Lambda(Vector(("out",SBox)), GT(Select(Ident("out"),"value").asIntValue, 0))))
    parse("Array(1,2).fold(0, fun (n1: Int, n2: Int) = n1 + n2)") shouldBe
      Apply(
        Select(Apply(Ident("Array"), Vector(LongConstant(1), LongConstant(2))), "fold"),
        Vector(LongConstant(0), Lambda(Vector(("n1",SLong), ("n2",SLong)), Plus(IntIdent("n1"), IntIdent("n2"))))
      )
    parse("OUTPUTS.slice(0, 10)") shouldBe
        Apply(Select(Ident("OUTPUTS"), "slice"), Vector(LongConstant(0), LongConstant(10)))
    parse("OUTPUTS.where(fun (out: Box) = out.value > 0)") shouldBe
        Apply(Select(Ident("OUTPUTS"), "where"),
          Vector(Lambda(Vector(("out",SBox)), GT(Select(Ident("out"),"value").asIntValue, 0))))
  }

  property("global functions") {
    parse("f(x)") shouldBe Apply(Ident("f"), IndexedSeq(Ident("x")))
    parse("f((x, y))") shouldBe Apply(Ident("f"), IndexedSeq(Tuple(IndexedSeq(Ident("x"), Ident("y")))))
    parse("f(x, y)") shouldBe Apply(Ident("f"), IndexedSeq(Ident("x"), Ident("y")))
    parse("f(x, y).size") shouldBe Select(Apply(Ident("f"), IndexedSeq(Ident("x"), Ident("y"))), "size")
    parse("f(x, y).get(1)") shouldBe Apply(Select(Apply(Ident("f"), IndexedSeq(Ident("x"), Ident("y"))), "get"), IndexedSeq(LongConstant(1)))
    parse("{let y = f(x); y}") shouldBe Block(Seq(Let("y", Apply(Ident("f"), IndexedSeq(Ident("x"))))), Ident("y"))
    parse("getVar[Array[Byte]](10)") shouldBe Apply(ApplyTypes(Ident("getVar"), Seq(SByteArray)), IndexedSeq(LongConstant(10)))
  }

  property("lambdas") {
    parse("fun (x: Int) = x + 1") shouldBe
      Lambda(IndexedSeq("x" -> SLong), Plus(Ident("x").asValue[SLong.type], LongConstant(1)))
    parse("fun (x: Int): Int = x + 1") shouldBe
      Lambda(IndexedSeq("x" -> SLong), SLong, Plus(Ident("x").asValue[SLong.type], LongConstant(1)))
    parse("fun (x: Int, box: Box): Int = x + box.value") shouldBe
        Lambda(IndexedSeq("x" -> SLong, "box" -> SBox), SLong,
               Plus(Ident("x").asValue[SLong.type], Select(Ident("box"), "value").asValue[SLong.type]))
    parse("fun (p: (Int, GroupElement), box: Box): Int = p._1 > box.value && p._2.isIdentity") shouldBe
        Lambda(IndexedSeq("p" -> STuple(SLong, SGroupElement), "box" -> SBox), SLong,
          AND(
            GT(Select(Ident("p"), "_1").asValue[SLong.type], Select(Ident("box"), "value").asValue[SLong.type]),
            Select(Select(Ident("p"), "_2"), "isIdentity").asValue[SBoolean.type]
            )
        )

    parse("fun (x) = x + 1") shouldBe
        Lambda(IndexedSeq("x" -> NoType), Plus(Ident("x").asValue[SLong.type], LongConstant(1)))
    parse("fun (x: Int) = { x + 1 }") shouldBe
        Lambda(IndexedSeq("x" -> SLong), Block(Seq(), Plus(Ident("x").asValue[SLong.type], LongConstant(1))))
    parse("fun (x: Int) = { let y = x + 1; y }") shouldBe
        Lambda(IndexedSeq("x" -> SLong),
          Block(Let("y", Plus(IntIdent("x"), 1)), Ident("y")))
  }

  property("predefined Exists with lambda argument") {
    parse("OUTPUTS.exists(fun (out: Box) = { out.amount >= minToRaise })") shouldBe
      Apply(Select(Ident("OUTPUTS"), "exists"),
        IndexedSeq(
          Lambda(IndexedSeq("out" -> SBox),
            Block(IndexedSeq(),
              GE(Select(Ident("out"), "amount").asValue[SLong.type], IntIdent("minToRaise"))))))
  }

  property("function definitions") {
    parse(
      """{let f = fun (x: Int) = x + 1
       |f}
      """.stripMargin) shouldBe
        Block(Let("f", Lambda(IndexedSeq("x" -> SLong), Plus(IntIdent("x"), 1))), Ident("f"))
    parse(
      """{fun f(x: Int) = x + 1
       |f}
      """.stripMargin) shouldBe
        Block(Let("f", Lambda(IndexedSeq("x" -> SLong), Plus(IntIdent("x"), 1))), Ident("f"))
  }

  property("get field of ref") {
    parse("XXX.YYY") shouldBe Select(Ident("XXX"), "YYY")
    parse("""
        |
        | X.Y
        |
      """.stripMargin) shouldBe Select(Ident("X"), "Y")
  }

  property("Box properties") {
    parse("fun (box: Box): Int = box.value") shouldBe Lambda(IndexedSeq("box" -> SBox), SLong, Select(Ident("box"), "value"))
    parse("fun (box: Box): Array[Byte] = box.propositionBytes") shouldBe Lambda(IndexedSeq("box" -> SBox), SByteArray, Select(Ident("box"), SBox.PropositionBytes))
    parse("fun (box: Box): Array[Byte] = box.bytes") shouldBe Lambda(IndexedSeq("box" -> SBox), SByteArray, Select(Ident("box"), "bytes"))
    parse("fun (box: Box): Array[Byte] = box.id") shouldBe Lambda(IndexedSeq("box" -> SBox), SByteArray, Select(Ident("box"), "id"))
  }

  property("type parameters") {
    parse("X[Byte]") shouldBe ApplyTypes(Ident("X"), Seq(SByte))
    parse("X[Int]") shouldBe ApplyTypes(Ident("X"), Seq(SLong))
    parse("X[Int].isDefined") shouldBe Select(ApplyTypes(Ident("X"), Seq(SLong)), "isDefined")
    parse("X[(Int, Boolean)]") shouldBe ApplyTypes(Ident("X"), Seq(STuple(SLong, SBoolean)))
    parse("X[Int, Boolean]") shouldBe ApplyTypes(Ident("X"), Seq(SLong, SBoolean))
    parse("SELF.R1[Int]") shouldBe ApplyTypes(Select(Ident("SELF"), "R1"), Seq(SLong))
    parse("SELF.R1[Int].isDefined") shouldBe Select(ApplyTypes(Select(Ident("SELF"), "R1"), Seq(SLong)),"isDefined")
    parse("f[Int](10)") shouldBe Apply(ApplyTypes(Ident("f"), Seq(SLong)), IndexedSeq(LongConstant(10)))
    parse("INPUTS.map[Int]") shouldBe ApplyTypes(Select(Ident("INPUTS"), "map"), Seq(SLong))
    parse("INPUTS.map[Int](10)") shouldBe Apply(ApplyTypes(Select(Ident("INPUTS"), "map"), Seq(SLong)), IndexedSeq(LongConstant(10)))
    parse("Array[Int]()") shouldBe Apply(ApplyTypes(Ident("Array"), Seq(SLong)), IndexedSeq.empty)
  }

  property("negative tests") {
    fail("(10", 3)
    fail("10)", 2)
    fail("X)", 1)
    fail("(X", 2)
    fail("{ X", 3)
    fail("{ let X", 7)
  }
}
