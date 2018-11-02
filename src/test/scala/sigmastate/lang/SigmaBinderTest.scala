package sigmastate.lang

import org.ergoplatform.{Height, Outputs, Self, Inputs}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{PropSpec, Matchers}
import scorex.util.encode.Base58
import sigmastate.Values._
import sigmastate._
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.{BinderException, InvalidTypeArguments, InvalidArguments}
import sigmastate.serialization.ValueSerializer
import sigmastate.utxo._

class SigmaBinderTest extends PropSpec with PropertyChecks with Matchers with LangTests {
  import StdSigmaBuilder._

  def bind(env: ScriptEnv, x: String): SValue = {
    val builder = TransformingSigmaBuilder
    val ast = SigmaParser(x, builder).get.value
    val binder = new SigmaBinder(env, builder)
    binder.bind(ast)
  }

  property("simple expressions") {
    bind(env, "x") shouldBe IntConstant(10)
    bind(env, "b1") shouldBe ByteConstant(1)
    bind(env, "x-y") shouldBe Minus(10, 11)
    bind(env, "x+y") shouldBe plus(10, 11)
    bind(env, "c1 && c2") shouldBe MethodCall(TrueLeaf, "&&", IndexedSeq(FalseLeaf))
    bind(env, "arr1") shouldBe ByteArrayConstant(Array(1, 2))
    bind(env, "HEIGHT - 1") shouldBe mkMinus(Height, 1)
    bind(env, "HEIGHT + 1") shouldBe plus(Height, 1)
    bind(env, "INPUTS.size > 1") shouldBe GT(Select(Inputs, "size").asIntValue, 1)
    bind(env, "arr1 | arr2") shouldBe Xor(Array[Byte](1, 2), Array[Byte](10, 20))
    bind(env, "arr1 ++ arr2") shouldBe MethodCall(Array[Byte](1, 2), "++", IndexedSeq(Array[Byte](10, 20))) // AppendBytes(Array[Byte](1, 2), Array[Byte](10,20))
    bind(env, "col1 ++ col2") shouldBe
      MethodCall(
        ConcreteCollection(LongConstant(1), LongConstant(2)),
        "++", IndexedSeq(ConcreteCollection(LongConstant(10), LongConstant(20))))
    bind(env, "g1 ^ n1") shouldBe Exponentiate(g1, n1)
    bind(env, "g1 * g2") shouldBe MethodCall(g1, "*", IndexedSeq(g2))
  }

  property("predefined functions") {
    bind(env, "getVar[Byte](10)") shouldBe GetVar(10.toByte, SByte)
    bind(env, "getVar[Byte](10L)") shouldBe GetVar(10.toByte, SByte)
    an[BinderException] should be thrownBy bind(env, "getVar[Byte](\"ha\")")
    bind(env, "min(1, 2)") shouldBe Min(IntConstant(1), IntConstant(2))
    bind(env, "max(1, 2)") shouldBe Max(IntConstant(1), IntConstant(2))
    bind(env, "min(1, 2L)") shouldBe Min(Upcast(IntConstant(1), SLong), LongConstant(2))
    an[InvalidArguments] should be thrownBy bind(env, "min(1, 2, 3)")
    an[InvalidArguments] should be thrownBy bind(env, "max(1)")
  }

  property("val constructs") {
    bind(env, "{val X = 10; X > 2}") shouldBe
      Block(Val("X", SInt, IntConstant(10)), GT(IntIdent("X"), 2))
    bind(env, "{val X = 10; X >= X}") shouldBe
      Block(Val("X", SInt, IntConstant(10)), GE(IntIdent("X"), IntIdent("X")))
    bind(env, "{val X = 10 - 1; X >= X}") shouldBe
      Block(Val("X", SInt, Minus(10, 1)), GE(IntIdent("X"), IntIdent("X")))
    bind(env, "{val X = 10 + 1; X >= X}") shouldBe
      Block(Val("X", NoType, plus(10, 1)), GE(IntIdent("X"), IntIdent("X")))
    bind(env,
      """{val X = 10
        |val Y = 11
        |X > Y}
      """.stripMargin) shouldBe Block(
      Seq(Val("X", SInt, IntConstant(10)), Val("Y", SInt, IntConstant(11))),
      GT(IntIdent("X"), IntIdent("Y")))
    bind(env, "{val X = (10, true); X._1 > 2 && X._2}") shouldBe
      Block(
        Val("X", STuple(SInt, SBoolean), Tuple(IntConstant(10), TrueLeaf)),
        MethodCall(GT(Select(IntIdent("X"), "_1").asValue[SInt.type], 2), "&&", IndexedSeq(Select(IntIdent("X"), "_2").asValue[SBoolean.type])))
  }

  property("predefined Exists with lambda argument") {
    val minToRaise = IntConstant(1000)
    val env = this.env ++ Map(
      "minToRaise" -> minToRaise
    )
    bind(env, "OUTPUTS.exists({ (out: Box) => out.amount >= minToRaise })") shouldBe
      Apply(Select(Outputs, "exists"),
        IndexedSeq(
          Lambda(IndexedSeq("out" -> SBox), NoType,
            GE(Select(Ident("out"), "amount").asValue[SInt.type], minToRaise))))
  }

  property("tuple constructor") {
    bind(env, "()") shouldBe UnitConstant
    bind(env, "(1)") shouldBe IntConstant(1)
    bind(env, "(1, 2)") shouldBe Tuple(IntConstant(1), IntConstant(2))
    bind(env, "(1, x - 1)") shouldBe Tuple(IntConstant(1), Minus(10, 1))
    bind(env, "(1, x + 1)") shouldBe Tuple(IntConstant(1), plus(10, 1))
    bind(env, "(1, 2, 3)") shouldBe Tuple(IntConstant(1), IntConstant(2), IntConstant(3))
    bind(env, "(1, 2 - 3, 4)") shouldBe Tuple(IntConstant(1), Minus(2, 3), IntConstant(4))
    bind(env, "(1, 2 + 3, 4)") shouldBe Tuple(IntConstant(1), plus(2, 3), IntConstant(4))
  }

  property("types") {
    bind(env, "{val X: Int = 10; 3 > 2}") shouldBe Block(Val("X", SInt, IntConstant(10)), GT(3, 2))
    bind(env, "{val X: (Int, Boolean) = (10, true); 3 > 2}") shouldBe
      Block(Val("X", STuple(SInt, SBoolean), Tuple(IntConstant(10), TrueLeaf)), GT(3, 2))
    bind(env, "{val X: Array[Int] = Array(1,2,3); X.size}") shouldBe
      Block(Val("X", SCollection(SInt), ConcreteCollection(IndexedSeq(IntConstant(1), IntConstant(2), IntConstant(3)))),
        Select(Ident("X"), "size"))
    bind(env, "{val X: (Array[Int], Box) = (Array(1,2,3), INPUT); X._1}") shouldBe
      Block(Val("X", STuple(SCollection(SInt), SBox), Tuple(ConcreteCollection(IndexedSeq(IntConstant(1), IntConstant(2), IntConstant(3))), Ident("INPUT"))),
        Select(Ident("X"), "_1"))
  }

  property("if") {
    bind(env, "if (true) x else y") shouldBe If(TrueLeaf, IntConstant(10), IntConstant(11))
    bind(env, "if(c1) 1 else if(x==y) 2 else 3") shouldBe
      If(TrueLeaf, IntConstant(1), If(EQ(IntConstant(10), IntConstant(11)), IntConstant(2), IntConstant(3)))
    bind(env,
      """if (true) { val A = x; 1 }
        |else if (x == y) 2 else 3""".stripMargin) shouldBe
      If(TrueLeaf,
        Block(Val("A", SInt, IntConstant(10)), IntConstant(1)),
        If(EQ(IntConstant(10), IntConstant(11)), IntConstant(2), IntConstant(3)))
  }

  property("Option constructors") {
    bind(env, "None") shouldBe NoneValue(NoType)
    bind(env, "Some(None)") shouldBe SomeValue(NoneValue(NoType))
    bind(env, "Some(10)") shouldBe SomeValue(IntConstant(10))
    bind(env, "Some(X)") shouldBe SomeValue(Ident("X"))
    bind(env, "Some(Some(X - 1))") shouldBe
      SomeValue(SomeValue(mkMinus(Ident("X").asValue[SInt.type], IntConstant(1))))
    bind(env, "Some(Some(X + 1))") shouldBe
      SomeValue(SomeValue(plus(Ident("X").asValue[SInt.type], IntConstant(1))))
  }

  property("lambdas") {
    bind(env, "{ (a: Int) => a - 1 }") shouldBe
      Lambda(IndexedSeq("a" -> SInt), NoType, mkMinus(IntIdent("a"), 1))
    bind(env, "{ (a: Int) => a + 1 }") shouldBe
      Lambda(IndexedSeq("a" -> SInt), NoType, plus(IntIdent("a"), 1))
    bind(env, "{ (a: Int, box: Box) => a - box.value }") shouldBe
      Lambda(IndexedSeq("a" -> SInt, "box" -> SBox), NoType,
        mkMinus(IntIdent("a"), Select(Ident("box"), "value").asValue[SLong.type]))
    bind(env, "{ (a) => a - 1 }") shouldBe
      Lambda(IndexedSeq("a" -> NoType), NoType, mkMinus(IntIdent("a"), IntConstant(1)))
    bind(env, "{ (a) => a - x }") shouldBe
      Lambda(IndexedSeq("a" -> NoType), NoType, mkMinus(IntIdent("a"), 10))
    bind(env, "{ (a: Int) => { val Y = a - 1; Y - x } }") shouldBe
      Lambda(IndexedSeq("a" -> SInt), NoType,
        Block(Val("Y", NoType, mkMinus(IntIdent("a"), 1)), mkMinus(IntIdent("Y"), 10)))
  }

  property("function definitions") {
    bind(env, "{val f = {(a: Int) => a - 1}; f}") shouldBe
      Block(Val("f", SFunc(IndexedSeq(SInt), NoType), Lambda(IndexedSeq("a" -> SInt), NoType, mkMinus(IntIdent("a"), 1))), Ident("f"))
  }

  property("predefined primitives") {
    bind(env, "{ (box: Box) => box.value }") shouldBe Lambda(IndexedSeq("box" -> SBox), NoType, Select(Ident("box"), "value"))
    bind(env, "{ (box: Box) => box.propositionBytes }") shouldBe Lambda(IndexedSeq("box" -> SBox), NoType, Select(Ident("box"), SBox.PropositionBytes))
    bind(env, "{ (box: Box) => box.bytes }") shouldBe Lambda(IndexedSeq("box" -> SBox), NoType, Select(Ident("box"), "bytes"))
    bind(env, "{ (box: Box) => box.id }") shouldBe Lambda(IndexedSeq("box" -> SBox), NoType, Select(Ident("box"), "id"))
  }

  property("type parameters") {
    bind(env, "X[Int]") shouldBe ApplyTypes(Ident("X"), Seq(SInt))
    bind(env, "X[Int].isDefined") shouldBe Select(ApplyTypes(Ident("X"), Seq(SInt)), "isDefined")
    bind(env, "X[(Int, Boolean)]") shouldBe ApplyTypes(Ident("X"), Seq(STuple(SInt, SBoolean)))
    bind(env, "X[Int, Boolean]") shouldBe ApplyTypes(Ident("X"), Seq(SInt, SBoolean))
    bind(env, "SELF.R1[Int]") shouldBe ApplyTypes(Select(Self, "R1"), Seq(SInt))
    bind(env, "SELF.R1[Int].isEmpty") shouldBe Select(ApplyTypes(Select(Self, "R1"), Seq(SInt)), "isEmpty")
    bind(env, "f[Int](10)") shouldBe Apply(ApplyTypes(Ident("f"), Seq(SInt)), IndexedSeq(IntConstant(10)))
    bind(env, "INPUTS.map[Int]") shouldBe ApplyTypes(Select(Inputs, "map"), Seq(SInt))
    bind(env, "INPUTS.map[Int](10)") shouldBe Apply(ApplyTypes(Select(Inputs, "map"), Seq(SInt)), IndexedSeq(IntConstant(10)))
    bind(env, "Array[Int]()") shouldBe ConcreteCollection()(SInt)
  }

  property("deserialize") {
    def roundtrip[T <: SType](c: EvaluatedValue[T], typeSig: String) = {
      val bytes = ValueSerializer.serialize(c)
      val str = Base58.encode(bytes)
      bind(env, s"deserialize[$typeSig](" + "\"" + str + "\")") shouldBe c
    }
    roundtrip(ByteArrayConstant(Array[Byte](2)), "Array[Byte]")
    roundtrip(Tuple(ByteArrayConstant(Array[Byte](2)), LongConstant(4)), "(Array[Byte], Long)")
  }

  property("deserialize fails") {
    // more than one type
    an[InvalidTypeArguments] should be thrownBy bind(env, """deserialize[Int, Byte]("test")""")
    // more then one argument
    an[InvalidArguments] should be thrownBy bind(env, """deserialize[Int]("test", "extra argument")""")
    // not a string constant
    an[InvalidArguments] should be thrownBy bind(env, """deserialize[Int]("a" + "b")""")
    // invalid chat in Base58 string
    an[AssertionError] should be thrownBy bind(env, """deserialize[Int]("0")""")
  }
}
