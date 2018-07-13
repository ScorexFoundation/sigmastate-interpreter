package sigmastate.lang

import fastparse.core.ParseError
import org.scalatest.exceptions.TestFailedException
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms._

class SigmaParserTest extends PropSpec with PropertyChecks with Matchers with LangTests {
  import StdSigmaBuilder._

  def parse(x: String): SValue = {
    val res = SigmaParser(x).get.value
    res
  }

  def parseType(x: String): SType = {
    val res = SigmaParser.parseType(x).get.value
    res
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
    parse("10") shouldBe IntConstant(10)
    parse("10L") shouldBe LongConstant(10)
    parse("10l") shouldBe LongConstant(10)
    parse("0x10") shouldBe IntConstant(0x10)
    parse("0x10L") shouldBe LongConstant(0x10)
    parse("0x10l") shouldBe LongConstant(0x10)
    parse("10L+11L") shouldBe Plus(10L, 11L)
    parse("(10+11)") shouldBe Plus(10, 11)
    parse("(10+11) + 12") shouldBe Plus(Plus(10, 11), 12)
    parse("10   + 11 + 12") shouldBe Plus(Plus(10, 11), 12)
    parse("1+2+3+4+5") shouldBe Plus(Plus(Plus(Plus(1, 2), 3), 4), 5)
    parse("10 - 11") shouldBe Minus(10, 11)
    parse("1 / 2") shouldBe Divide(1, 2)
    parse("5 % 2") shouldBe Modulo(5, 2)
    parse("1==1") shouldBe EQ(1, 1)
    parse("true && true") shouldBe AND(TrueLeaf, TrueLeaf)
    parse("true || false") shouldBe OR(TrueLeaf, FalseLeaf)
    parse("true || (true && false)") shouldBe OR(TrueLeaf, AND(TrueLeaf, FalseLeaf))
    parse("false || false || false") shouldBe OR(OR(FalseLeaf, FalseLeaf), FalseLeaf)
    parse("(1>= 0)||(3L >2L)") shouldBe OR(GE(1, 0), GT(3L, 2L))
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

  property("tuple operations") {
    parse("()") shouldBe UnitConstant
    parse("(1)") shouldBe IntConstant(1)
    parse("(1, 2)") shouldBe Tuple(IntConstant(1), IntConstant(2))
    parse("(1, X + 1)") shouldBe Tuple(IntConstant(1), mkPlus(IntIdent("X"), 1))
    parse("(1, 2, 3)") shouldBe Tuple(IntConstant(1), IntConstant(2), IntConstant(3))
    parse("(1, 2 + 3, 4)") shouldBe Tuple(IntConstant(1), Plus(2, 3), IntConstant(4))

    parse("(1, 2L)._1") shouldBe Select(Tuple(IntConstant(1), LongConstant(2)), "_1")
    parse("(1, 2L)._2") shouldBe Select(Tuple(IntConstant(1), LongConstant(2)), "_2")
    parse("(1, 2L, 3)._3") shouldBe Select(Tuple(IntConstant(1), LongConstant(2), IntConstant(3)), "_3")

    // tuple as collection
    parse("(1, 2L).size") shouldBe Select(Tuple(IntConstant(1), LongConstant(2)), "size")
    parse("(1, 2L)(0)") shouldBe Apply(Tuple(IntConstant(1), LongConstant(2)), IndexedSeq(IntConstant(0)))
    parse("(1, 2L).getOrElse(2, 3)") shouldBe Apply(Select(Tuple(IntConstant(1), LongConstant(2)), "getOrElse"), IndexedSeq(IntConstant(2), IntConstant(3)))
  }

  property("let constructs") {
    parse(
      """{let X = 10
        |3 > 2}
      """.stripMargin) shouldBe Block(Let("X", IntConstant(10)), GT(3, 2))

    parse("{let X = 10; 3 > 2}") shouldBe Block(Let("X", IntConstant(10)), GT(3, 2))
    parse("{let X = 3 + 2; 3 > 2}") shouldBe Block(Let("X", Plus(3, 2)), GT(3, 2))
    parse("{let X = if (true) true else false; false}") shouldBe Block(Let("X", If(TrueLeaf, TrueLeaf, FalseLeaf)), FalseLeaf)

    val expr = parse(
      """{let X = 10
        |let Y = 11
        |X > Y}
      """.stripMargin)

    expr shouldBe Block(Seq(Let("X", IntConstant(10)),Let("Y", IntConstant(11))), GT(IntIdent("X"), IntIdent("Y")))
  }

  property("types") {
    parse("{let X: Byte = 10; 3 > 2}") shouldBe Block(Seq(Let("X", SByte, IntConstant(10))), GT(3, 2))
    parse("{let X: Int = 10; 3 > 2}") shouldBe Block(Seq(Let("X", SInt, IntConstant(10))), GT(3, 2))
    parse("""{let X: (Int, Boolean) = (10, true); 3 > 2}""") shouldBe
      Block(Seq(Let("X", STuple(SInt, SBoolean), Tuple(IntConstant(10), TrueLeaf))), GT(3, 2))
    parse("""{let X: Array[Int] = Array(1,2,3); X.size}""") shouldBe
      Block(Seq(Let("X", SCollection(SInt), Apply(Ident("Array"), IndexedSeq(IntConstant(1), IntConstant(2), IntConstant(3))))),
            Select(Ident("X"), "size"))
    parse("""{let X: (Array[Int], Box) = (Array(1,2,3), INPUT); X._1}""") shouldBe
        Block(Seq(Let("X", STuple(SCollection(SInt), SBox), Tuple(Apply(Ident("Array"), IndexedSeq(IntConstant(1), IntConstant(2), IntConstant(3))), Ident("INPUT")))),
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
      """.stripMargin) shouldBe Block(Seq(Let("X", IntConstant(10))), TrueLeaf)
    parse(
      """{let X = 11
        |true}
      """.stripMargin) shouldBe Block(Seq(Let("X", IntConstant(11))), TrueLeaf)
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
      """.stripMargin) shouldBe Block(Seq(Let("X", IntConstant(12))), Plus(3, 2))
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
        |else if ( X == Y) 2 else 3""".stripMargin) shouldBe
        If(TrueLeaf,
          Block(Seq(Let("A", IntConstant(10))), IntConstant(1)),
          If(EQ(Ident("X"), Ident("Y")), IntConstant(2), IntConstant(3))
    )

  }

  property("array literals") {
    val emptyCol = Apply(Ident("Array"), IndexedSeq.empty)
    parse("Array()") shouldBe emptyCol
    val emptyCol2 = Apply(Ident("Array"), IndexedSeq(emptyCol))
    parse("Array(Array())") shouldBe emptyCol2
    parse("Array(Array(Array()))") shouldBe Apply(Ident("Array"), IndexedSeq(emptyCol2))

    parse("Array(1)") shouldBe Apply(Ident("Array"), IndexedSeq(IntConstant(1)))
    parse("Array(1, X)") shouldBe Apply(Ident("Array"), IndexedSeq(IntConstant(1), Ident("X")))
    parse("Array(1, X + 1, Array())") shouldBe
    Apply(Ident("Array"),
      IndexedSeq(
        IntConstant(1),
        mkPlus(Ident("X").asValue[SLong.type], IntConstant(1)),
        Apply(Ident("Array"), IndexedSeq.empty)))
    parse("Array(Array(X + 1))") shouldBe Apply(Ident("Array"),
      IndexedSeq(Apply(Ident("Array"), IndexedSeq(
                  mkPlus(Ident("X").asValue[SLong.type], IntConstant(1))))))
  }

  property("Option constructors") {
    parse("None") shouldBe Ident("None")
    parse("Some(None)") shouldBe Apply(Ident("Some"), IndexedSeq(Ident("None")))
    parse("Some(10)") shouldBe Apply(Ident("Some"), IndexedSeq(IntConstant(10)))
    parse("Some(X)") shouldBe Apply(Ident("Some"), IndexedSeq(Ident("X")))
    parse("Some(Some(X + 1))") shouldBe Apply(Ident("Some"),
      IndexedSeq(Apply(Ident("Some"), IndexedSeq(
                  mkPlus(Ident("X").asValue[SLong.type], IntConstant(1))))))
  }

  property("array indexed access") {
    parse("Array()") shouldBe Apply(Ident("Array"), IndexedSeq.empty)
    parse("Array()(0)") shouldBe Apply(Apply(Ident("Array"), IndexedSeq.empty), IndexedSeq(IntConstant(0)))
    parse("Array()(0)(0)") shouldBe Apply(Apply(Apply(Ident("Array"), IndexedSeq.empty), IndexedSeq(IntConstant(0))), IndexedSeq(IntConstant(0)))
  }

  property("array indexed access with default values") {
    parse("Array()(0, 1)") shouldBe
      Apply(Apply(Ident("Array"), IndexedSeq.empty), IndexedSeq(IntConstant(0), IntConstant(1)))
    parse("Array()(0, 1)(0)") shouldBe
      Apply(Apply(Apply(Ident("Array"), IndexedSeq.empty),
        IndexedSeq(IntConstant(0), IntConstant(1))),
        IndexedSeq(IntConstant(0)))
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
        Select(Apply(Ident("Array"), Vector(IntConstant(1), IntConstant(2))), "fold"),
        Vector(IntConstant(0), Lambda(Vector(("n1",SInt), ("n2",SInt)), Plus(IntIdent("n1"), IntIdent("n2"))))
      )
    parse("OUTPUTS.slice(0, 10)") shouldBe
        Apply(Select(Ident("OUTPUTS"), "slice"), Vector(IntConstant(0), IntConstant(10)))
    parse("OUTPUTS.where(fun (out: Box) = out.value > 0)") shouldBe
        Apply(Select(Ident("OUTPUTS"), "where"),
          Vector(Lambda(Vector(("out",SBox)), GT(Select(Ident("out"),"value").asIntValue, 0))))
  }

  property("global functions") {
    parse("f(x)") shouldBe Apply(Ident("f"), IndexedSeq(Ident("x")))
    parse("f((x, y))") shouldBe Apply(Ident("f"), IndexedSeq(Tuple(IndexedSeq(Ident("x"), Ident("y")))))
    parse("f(x, y)") shouldBe Apply(Ident("f"), IndexedSeq(Ident("x"), Ident("y")))
    parse("f(x, y).size") shouldBe Select(Apply(Ident("f"), IndexedSeq(Ident("x"), Ident("y"))), "size")
    parse("f(x, y).get(1)") shouldBe Apply(Select(Apply(Ident("f"), IndexedSeq(Ident("x"), Ident("y"))), "get"), IndexedSeq(IntConstant(1)))
    parse("{let y = f(x); y}") shouldBe Block(Seq(Let("y", Apply(Ident("f"), IndexedSeq(Ident("x"))))), Ident("y"))
    parse("getVar[Array[Byte]](10)") shouldBe Apply(ApplyTypes(Ident("getVar"), Seq(SByteArray)), IndexedSeq(IntConstant(10)))
  }

  property("lambdas") {
    parse("fun (x: Int) = x + 1") shouldBe
      Lambda(IndexedSeq("x" -> SInt), mkPlus(Ident("x").asValue[SInt.type], IntConstant(1)))
    parse("fun (x: Int): Int = x + 1") shouldBe
      Lambda(IndexedSeq("x" -> SInt), SInt, mkPlus(Ident("x").asValue[SInt.type], IntConstant(1)))
    parse("fun (x: Int, box: Box): Long = x + box.value") shouldBe
        Lambda(IndexedSeq("x" -> SInt, "box" -> SBox), SLong,
               Plus(Ident("x").asValue[SInt.type], Select(Ident("box"), "value").asValue[SLong.type]))
    parse("fun (p: (Int, GroupElement), box: Box): Boolean = p._1 > box.value && p._2.isIdentity") shouldBe
        Lambda(IndexedSeq("p" -> STuple(SInt, SGroupElement), "box" -> SBox), SBoolean,
          AND(
            GT(Select(Ident("p"), "_1").asValue[SInt.type], Select(Ident("box"), "value").asValue[SLong.type]),
            Select(Select(Ident("p"), "_2"), "isIdentity").asValue[SBoolean.type]
            )
        )

    parse("fun (x) = x + 1") shouldBe
        Lambda(IndexedSeq("x" -> NoType), mkPlus(Ident("x").asValue[SInt.type], IntConstant(1)))
    parse("fun (x: Int) = { x + 1 }") shouldBe
        Lambda(IndexedSeq("x" -> SInt), Block(Seq(), mkPlus(Ident("x").asValue[SInt.type], IntConstant(1))))
    parse("fun (x: Int) = { let y = x + 1; y }") shouldBe
        Lambda(IndexedSeq("x" -> SInt),
          Block(Let("y", mkPlus(IntIdent("x"), 1)), Ident("y")))
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
        Block(Let("f", Lambda(IndexedSeq("x" -> SInt), mkPlus(IntIdent("x"), 1))), Ident("f"))
    parse(
      """{fun f(x: Int) = x + 1
       |f}
      """.stripMargin) shouldBe
        Block(Let("f", Lambda(IndexedSeq("x" -> SInt), mkPlus(IntIdent("x"), 1))), Ident("f"))
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
    parse("fun (box: Box): Long = box.value") shouldBe Lambda(IndexedSeq("box" -> SBox), SLong, Select(Ident("box"), "value"))
    parse("fun (box: Box): Array[Byte] = box.propositionBytes") shouldBe Lambda(IndexedSeq("box" -> SBox), SByteArray, Select(Ident("box"), SBox.PropositionBytes))
    parse("fun (box: Box): Array[Byte] = box.bytes") shouldBe Lambda(IndexedSeq("box" -> SBox), SByteArray, Select(Ident("box"), "bytes"))
    parse("fun (box: Box): Array[Byte] = box.id") shouldBe Lambda(IndexedSeq("box" -> SBox), SByteArray, Select(Ident("box"), "id"))
  }

  property("type parameters") {
    parse("X[Byte]") shouldBe ApplyTypes(Ident("X"), Seq(SByte))
    parse("X[Int]") shouldBe ApplyTypes(Ident("X"), Seq(SInt))
    parse("X[Int].isDefined") shouldBe Select(ApplyTypes(Ident("X"), Seq(SInt)), "isDefined")
    parse("X[(Int, Boolean)]") shouldBe ApplyTypes(Ident("X"), Seq(STuple(SInt, SBoolean)))
    parse("X[Int, Boolean]") shouldBe ApplyTypes(Ident("X"), Seq(SInt, SBoolean))
    parse("SELF.R1[Int]") shouldBe ApplyTypes(Select(Ident("SELF"), "R1"), Seq(SInt))
    parse("SELF.R1[Int].isDefined") shouldBe Select(ApplyTypes(Select(Ident("SELF"), "R1"), Seq(SInt)),"isDefined")
    parse("f[Int](10)") shouldBe Apply(ApplyTypes(Ident("f"), Seq(SInt)), IndexedSeq(IntConstant(10)))
    parse("INPUTS.map[Int]") shouldBe ApplyTypes(Select(Ident("INPUTS"), "map"), Seq(SInt))
    parse("INPUTS.map[Int](10)") shouldBe Apply(ApplyTypes(Select(Ident("INPUTS"), "map"), Seq(SInt)), IndexedSeq(IntConstant(10)))
    parse("Array[Int]()") shouldBe Apply(ApplyTypes(Ident("Array"), Seq(SInt)), IndexedSeq.empty)
  }

  property("type tests") {
    parseType("Int") shouldBe SInt
    parseType("(Int, Long)") shouldBe STuple(SInt, SLong)
    parseType("Array[(Int, Long)]") shouldBe SCollection(STuple(SInt, SLong))
    parseType("Array[(Array[Byte], Long)]") shouldBe SCollection(STuple(SCollection(SByte), SLong))
    parseType("Array[(Array[Byte], Array[Long])]") shouldBe SCollection(STuple(SCollection(SByte), SCollection(SLong)))
    parseType("Array[(Array[Byte], (Array[Long], Long))]") shouldBe SCollection(STuple(SCollection(SByte), STuple(SCollection(SLong), SLong)))
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
