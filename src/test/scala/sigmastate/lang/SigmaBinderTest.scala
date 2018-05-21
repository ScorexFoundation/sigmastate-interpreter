package sigmastate.lang

import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import sigmastate._
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate.utxo._

class SigmaBinderTest extends PropSpec with PropertyChecks with Matchers with LangTests {

  def bind(env: Map[String, Any], x: String): SValue = {
    val ast = SigmaParser(x).get.value
    val binder = new SigmaBinder(env)
    binder.bind(ast)
  }

  property("simple expressions") {
    bind(env, "x") shouldBe IntConstant(10)
    bind(env, "b1") shouldBe ByteConstant(1)
    bind(env, "x+y") shouldBe Plus(10, 11)
    bind(env, "c1 && c2") shouldBe AND(TrueLeaf, FalseLeaf)
    bind(env, "arr1") shouldBe ByteArrayConstant(Array(1, 2))
    bind(env, "HEIGHT + 1") shouldBe Plus(Height, 1)
    bind(env, "INPUTS.size > 1") shouldBe GT(Select(Inputs, "size").asIntValue, 1)
    bind(env, "arr1 | arr2") shouldBe Xor(Array[Byte](1, 2), Array[Byte](10,20))
    bind(env, "arr1 ++ arr2") shouldBe MethodCall(Array[Byte](1, 2), "++", IndexedSeq(Array[Byte](10,20)))  // AppendBytes(Array[Byte](1, 2), Array[Byte](10,20))
    bind(env, "col1 ++ col2") shouldBe
      MethodCall(
        ConcreteCollection(IntConstant(1), IntConstant(2)),
        "++", IndexedSeq(ConcreteCollection(IntConstant(10), IntConstant(20))))
    bind(env, "g1 ^ n") shouldBe Exponentiate(g1, n)
    bind(env, "g1 * g2") shouldBe MultiplyGroup(g1, g2)
  }

  property("predefined functions") {
    bind(env, "allOf(Array(c1, c2))") shouldBe
        AND(ConcreteCollection(Vector(TrueLeaf, FalseLeaf)))
    bind(env, "getVar[Byte](10)") shouldBe TaggedVariable(10, SByte)
    bind(env, "getVar[Array[Byte]](10)") shouldBe TaggedVariable(10, SByteArray)
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
    bind(env, "{let X = (10, true); X._1 > 2 && X._2}") shouldBe
        Block(
          Let("X", STuple(SInt, SBoolean), Tuple(IntConstant(10), TrueLeaf)),
          AND(GT(Select(IntIdent("X"), "_1").asValue[SInt.type], 2), Select(IntIdent("X"), "_2").asValue[SBoolean.type]))
  }

  property("predefined Exists with lambda argument") {
    val minToRaise = IntConstant(1000)
    val env = this.env ++ Map(
      "minToRaise" -> minToRaise,
    )
    bind(env, "OUTPUTS.exists(fun (out: Box) = { out.amount >= minToRaise })") shouldBe
      Apply(Select(Outputs, "exists"),
      IndexedSeq(
        Lambda(IndexedSeq("out" -> SBox), SBoolean,
            GE(Select(Ident("out"), "amount").asValue[SInt.type], minToRaise))))
  }

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

  property("Option constructors") {
    bind(env, "None") shouldBe NoneValue(NoType)
    bind(env, "Some(None)") shouldBe SomeValue(NoneValue(NoType))
    bind(env, "Some(10)") shouldBe SomeValue(IntConstant(10))
    bind(env, "Some(X)") shouldBe SomeValue(Ident("X"))
    bind(env, "Some(Some(X + 1))") shouldBe
      SomeValue(SomeValue(Plus(Ident("X").asValue[SInt.type], IntConstant(1))))
  }

  property("array indexed access") {
    bind(env, "Array(1)(0)") shouldBe ByIndex(ConcreteCollection(IndexedSeq(IntConstant(1)))(SInt), 0)
    bind(env, "Array(Array(1))(0)(0)") shouldBe
      ByIndex(ByIndex(ConcreteCollection(IndexedSeq(ConcreteCollection(IndexedSeq(IntConstant(1)))))(SCollection(SInt)), 0), 0)
  }

  property("array indexed access with default value") {
    bind(env, "Array(1)(0, 1)") shouldBe
      ByIndex(ConcreteCollection(IndexedSeq(IntConstant(1)))(SInt), 0, Some(IntConstant(1)))

    bind(env, "Array(Array(1))(0, Array(2))(0)") shouldBe
      ByIndex(
        ByIndex(
          ConcreteCollection(IndexedSeq(ConcreteCollection(IndexedSeq(IntConstant(1)))))(SCollection(SInt)),
          0,
          Some(ConcreteCollection(Vector(IntConstant(2))))),
        0)
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

  property("function definitions") {
    bind(env, "{let f = fun (a: Int) = a + 1; f}") shouldBe
        Block(Let("f", SFunc(IndexedSeq(SInt), SInt), Lambda(IndexedSeq("a" -> SInt), SInt, Plus(IntIdent("a"), 1))), Ident("f"))
    bind(env, "{fun f(a: Int) = a + x; f}") shouldBe
        Block(Let("f", SFunc(IndexedSeq(SInt), SInt), Lambda(IndexedSeq("a" -> SInt), SInt, Plus(IntIdent("a"), 10))), Ident("f"))
  }

  property("predefined primitives") {
    bind(env, "fun (box: Box): Int = box.value") shouldBe Lambda(IndexedSeq("box" -> SBox), SInt, Select(Ident("box"), "value"))
    bind(env, "fun (box: Box): Array[Byte] = box.propositionBytes") shouldBe Lambda(IndexedSeq("box" -> SBox), SByteArray, Select(Ident("box"), SBox.PropositionBytes))
    bind(env, "fun (box: Box): Array[Byte] = box.bytes") shouldBe Lambda(IndexedSeq("box" -> SBox), SByteArray, Select(Ident("box"), "bytes"))
    bind(env, "fun (box: Box): Array[Byte] = box.id") shouldBe Lambda(IndexedSeq("box" -> SBox), SByteArray, Select(Ident("box"), "id"))
  }

  property("type parameters") {
    bind(env, "X[Int]") shouldBe ApplyTypes(Ident("X"), Seq(SInt))
    bind(env, "X[Int].isDefined") shouldBe Select(ApplyTypes(Ident("X"), Seq(SInt)), "isDefined")
    bind(env, "X[(Int, Boolean)]") shouldBe ApplyTypes(Ident("X"), Seq(STuple(SInt, SBoolean)))
    bind(env, "X[Int, Boolean]") shouldBe ApplyTypes(Ident("X"), Seq(SInt, SBoolean))
    bind(env, "SELF.R1[Int]") shouldBe ApplyTypes(Select(Self, "R1"), Seq(SInt))
    bind(env, "SELF.R1[Int].isDefined") shouldBe Select(ApplyTypes(Select(Self, "R1"), Seq(SInt)),"isDefined")
    bind(env, "f[Int](10)") shouldBe Apply(ApplyTypes(Ident("f"), Seq(SInt)), IndexedSeq(IntConstant(10)))
    bind(env, "INPUTS.map[Int]") shouldBe ApplyTypes(Select(Inputs, "map"), Seq(SInt))
    bind(env, "INPUTS.map[Int](10)") shouldBe Apply(ApplyTypes(Select(Inputs, "map"), Seq(SInt)), IndexedSeq(IntConstant(10)))
    bind(env, "Array[Int]()") shouldBe ConcreteCollection()(SInt)
  }

}
