package sigmastate.lang

import org.ergoplatform.ErgoAddressEncoder.{NetworkPrefix, TestnetNetworkPrefix}
import org.ergoplatform.ErgoBox.R4
import org.ergoplatform._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scapi.sigma.DLogProtocol.ProveDlog
import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms.Ident
import sigmastate.lang.exceptions.SpecializerException
import sigmastate.serialization.generators.{ConcreteCollectionGenerators, TransformerGenerators, ValueGenerators}
import sigmastate.utxo._

class SigmaSpecializerTest extends PropSpec
  with PropertyChecks
  with Matchers
  with LangTests
  with ValueGenerators
  with ConcreteCollectionGenerators
  with TransformerGenerators {

  private def countANDORInputNodes(root: Transformer[SCollection[SBoolean.type], SBoolean.type]): Int =
    root.input.items.foldLeft(0) { (sum, item) =>
      item match {
        case r@AND(_) => sum + countANDORInputNodes(r)
        case r@OR(_) => sum + countANDORInputNodes(r)
        case _ => sum + 1
      }
    }

  def typed(env: Map[String, SValue], x: String): SValue = {
    val builder = TransformingSigmaBuilder
    val parsed = SigmaParser(x, builder).get.value
    val binder = new SigmaBinder(env, builder)
    val bound = binder.bind(parsed)
    val typer = new SigmaTyper(builder)
    val typed = typer.typecheck(bound)
    typed
  }
  def spec(env: Map[String, SValue], typed: SValue, networkPrefix: NetworkPrefix = TestnetNetworkPrefix): SValue = {
    val spec = new SigmaSpecializer(TransformingSigmaBuilder, networkPrefix)
    spec.specialize(env, typed)
  }
  def spec(code: String): SValue = {
    spec(Map(), typed(Map(), code))
  }
  def fail(env: Map[String, SValue], code: String, messageSubstr: String = ""): Unit = {
    try {
      spec(env, typed(env, code))
      assert(false, s"Should fail: $code")
    } catch {
      case e: SpecializerException =>
        if (messageSubstr.nonEmpty)
          if (!e.getMessage.contains(messageSubstr)) {
            throw new AssertionError(s"Error message '${e.getMessage}' doesn't contain '$messageSubstr'.", e)
          }
    }
  }

  property("resolve let-bound names and substitute") {
    spec(Map("X" -> LongConstant(10)),
         Ident("X", SLong)) shouldBe LongConstant(10)
    spec(Map("X" -> LongConstant(10)),
         Plus(Ident("X", SLong).asValue[SLong.type], LongConstant(1))) shouldBe Plus(10L, 1L)
  }

  property("substitute all val expressions in block result") {
    spec("{ val X = 10; X }") shouldBe IntConstant(10)
    spec("{ val X = 10; val Y = 20; X + Y }") shouldBe Plus(10, 20)
    spec("{ val X = 10; val Y = 20; X + Y + X }") shouldBe Plus(Plus(10, 20), 10)
    spec("{ val X = 10 + 1; X + X}") shouldBe Plus(Plus(10, 1), Plus(10, 1))
    spec("{ val X = 10; val Y = X; Y}") shouldBe IntConstant(10)
    spec("{ val X = 10; val Y = X; val Z = Y; Z }") shouldBe IntConstant(10)
    spec("{ val X = 10; val Y = X + 1; val Z = Y + X; Z + Y + X }") shouldBe
      Plus(Plus(/*Z=*/Plus(/*Y=*/Plus(10, 1), 10), /*Y=*/Plus(10, 1)), 10)
  }

  property("Tuple operations") {
    spec("(1, 2L)._1") shouldBe SelectField(Tuple(IntConstant(1), LongConstant(2L)), 1)
    spec("(1, 2L)._2") shouldBe SelectField(Tuple(IntConstant(1), LongConstant(2L)), 2)
    spec("(1, 2L, 3)._3") shouldBe SelectField(Tuple(IntConstant(1), LongConstant(2L), IntConstant(3)), 3)

    // tuple as collection
    spec("(1, 2L).size") shouldBe SizeOf(Tuple(IntConstant(1), LongConstant(2L)))
//    spec(env, "(1, 2L)(0)") shouldBe SInt
//    spec(env, "(1, 2L)(1)") shouldBe SLong
//    spec(env, "(1, 2L).getOrElse(2, 3)") shouldBe SAny
//    spec(env, "(1, 2L).slice(0, 2)") shouldBe SCollection(SAny)
//    spec(env, "fun (a: Int) = (1, 2L)(a)") shouldBe SFunc(IndexedSeq(SInt), SAny)
  }
  
  property("Option constructors") {
    fail(Map(), "None", "Option constructors are not supported")
    fail(Map(), "Some(10)", "Option constructors are not supported")
  }

  property("generic methods of arrays") {
    spec("OUTPUTS.map({ (out: Box) => out.value >= 10 })") shouldBe
      MapCollection(Outputs, 21, GE(ExtractAmount(TaggedBox(21)), LongConstant(10)))
    spec("OUTPUTS.exists({ (out: Box) => out.value >= 10 })") shouldBe
        Exists(Outputs, 21, GE(ExtractAmount(TaggedBox(21)), LongConstant(10)))
    spec("OUTPUTS.forall({ (out: Box) => out.value >= 10 })") shouldBe
        ForAll(Outputs, 21, GE(ExtractAmount(TaggedBox(21)), LongConstant(10)))
    spec("{ val arr = Array(1,2); arr.fold(0, { (n1: Int, n2: Int) => n1 + n2 })}") shouldBe
        Fold(ConcreteCollection(IntConstant(1), IntConstant(2)),
             22, IntConstant(0), 21, Plus(TaggedInt(21), TaggedInt(22)))
    spec("{ val arr = Array(1,2); arr.fold(true, {(n1: Boolean, n2: Int) => n1 && (n2 > 1)})}") shouldBe
      Fold(ConcreteCollection(IntConstant(1), IntConstant(2)),
        22, TrueLeaf, 21, BinAnd(TaggedBoolean(21), GT(TaggedInt(22), IntConstant(1))))
    spec("OUTPUTS.slice(0, 10)") shouldBe
        Slice(Outputs, IntConstant(0), IntConstant(10))
    spec("OUTPUTS.filter({ (out: Box) => out.value >= 10 })") shouldBe
        Filter(Outputs, 21, GE(ExtractAmount(TaggedBox(21)), LongConstant(10)))
  }

  property("AND flattening predefined") {
    spec("true && true") shouldBe BinAnd(TrueLeaf, TrueLeaf)
    spec("true && false") shouldBe BinAnd(TrueLeaf, FalseLeaf)
    spec("true && (true && 10 == 10)") shouldBe
      BinAnd(TrueLeaf, BinAnd(TrueLeaf, EQ(IntConstant(10), IntConstant(10))))
    spec("true && true && true") shouldBe BinAnd(BinAnd(TrueLeaf, TrueLeaf), TrueLeaf)
    spec("true && (true && (true && true)) && true") shouldBe
      BinAnd(BinAnd(TrueLeaf, BinAnd(TrueLeaf, BinAnd(TrueLeaf, TrueLeaf))), TrueLeaf)
  }

  property("AND flattening, CAND/COR untouched") {
    val sigmaBooleans1 = AND(Seq(TrueLeaf, CAND(Seq(proveDlogGen.sample.get, proveDHTGen.sample.get))))
    spec(Map(), sigmaBooleans1) shouldBe sigmaBooleans1
    val sigmaBooleans2 = AND(Seq(TrueLeaf, COR(Seq(proveDlogGen.sample.get, proveDHTGen.sample.get))))
    spec(Map(), sigmaBooleans2) shouldBe sigmaBooleans2
  }

  property("AND flattening") {
    forAll(logicalExprTreeNodeGen(Seq(AND.apply))) { tree =>
      val out = spec(Map(), tree).asInstanceOf[AND]
      out.input.items.forall(!_.isInstanceOf[AND]) shouldBe true
      countANDORInputNodes(tree) shouldBe out.input.items.length
    }
  }

  property("OR flattening predefined") {
    spec("true || true || true") shouldBe BinOr(BinOr(TrueLeaf, TrueLeaf), TrueLeaf)
    spec("true || (true || true) || true") shouldBe
      BinOr(BinOr(TrueLeaf, BinOr(TrueLeaf, TrueLeaf)), TrueLeaf)
  }

  property("OR flattening, CAND/COR untouched") {
    val sigmaBooleans1 = OR(Seq(TrueLeaf, CAND(Seq(proveDlogGen.sample.get, proveDHTGen.sample.get))))
    spec(Map(), sigmaBooleans1) shouldBe sigmaBooleans1
    val sigmaBooleans2 = OR(Seq(TrueLeaf, COR(Seq(proveDlogGen.sample.get, proveDHTGen.sample.get))))
    spec(Map(), sigmaBooleans2) shouldBe sigmaBooleans2
  }

  property("OR flattening") {
    forAll(logicalExprTreeNodeGen(Seq(OR.apply))) { tree =>
      val out = spec(Map(), tree).asInstanceOf[OR]
      out.input.items.forall(!_.isInstanceOf[OR]) shouldBe true
      countANDORInputNodes(tree) shouldBe out.input.items.length
    }
  }

  property("numeric casts") {
    spec("1.toByte") shouldBe ByteConstant(1)
    spec("1.toLong") shouldBe LongConstant(1)
    spec("1.toBigInt") shouldBe BigIntConstant(1)
    spec("HEIGHT.toLong") shouldBe Height
    spec("HEIGHT.toByte") shouldBe Downcast(Height, SByte)
    spec("INPUTS.size.toLong") shouldBe Upcast(SizeOf(Inputs), SLong)
    spec("INPUTS.size.toBigInt") shouldBe Upcast(SizeOf(Inputs), SBigInt)
  }

  property("failed numeric casts for constants") {
    an[ArithmeticException] should be thrownBy spec("999.toByte")
    an[ArithmeticException] should be thrownBy spec("999.toShort.toByte")
    an[ArithmeticException] should be thrownBy spec(s"${Int.MaxValue}.toShort")
    an[ArithmeticException] should be thrownBy spec(s"${Long.MaxValue}L.toInt")
  }

  property("byteArrayToBigInt") {
    spec("byteArrayToBigInt(longToByteArray(1L))") shouldBe ByteArrayToBigInt(LongToByteArray(LongConstant(1)))
  }

  property("fromBaseX") {
    spec(""" fromBase58("r") """) shouldBe ByteArrayConstant(Array(49))
    spec(""" fromBase64("MQ") """) shouldBe ByteArrayConstant(Array(49))
    spec(""" fromBase64("M" + "Q") """) shouldBe ByteArrayConstant(Array(49))
  }

  property("failed fromBaseX (invalid input)") {
    an[AssertionError] should be thrownBy spec(""" fromBase58("^%$#@")""")
    an[IllegalArgumentException] should be thrownBy spec(""" fromBase64("^%$#@")""")
  }

  private def testPK(networkPrefix: NetworkPrefix) = {
    implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(networkPrefix)
    val dk1 = proveDlogGen.sample.get
    val encodedP2PK = P2PKAddress(dk1).toString
    val code = s"""PK("$encodedP2PK")"""
    spec(Map(), typed(Map(), code), networkPrefix) shouldEqual dk1
  }

  property("PK (testnet network prefix)") {
    testPK(TestnetNetworkPrefix)
  }

  property("PK (mainnet network prefix)") {
    testPK(ErgoAddressEncoder.MainnetNetworkPrefix)
  }

  property("ExtractRegisterAs") {
    spec("SELF.R4[Int]") shouldBe ExtractRegisterAs[SInt.type](Self, R4)
  }

  property("OptionIsDefined") {
    spec("SELF.R4[Int].isDefined") shouldBe ExtractRegisterAs[SInt.type](Self, R4).isDefined
    spec("getVar[Int](1).isDefined") shouldBe GetVarInt(1).isDefined
  }

  property("OptionGet") {
    spec("SELF.R4[Int].get") shouldBe ExtractRegisterAs[SInt.type](Self, R4).get
    spec("getVar[Int](1).get") shouldBe GetVarInt(1).get
  }

  property("OptionGetOrElse") {
    spec("SELF.R4[Int].getOrElse(0)") shouldBe ExtractRegisterAs[SInt.type](Self, R4).getOrElse(IntConstant(0))
    spec("getVar[Int](1).getOrElse(0)") shouldBe GetVarInt(1).getOrElse(IntConstant(0))
  }

  property("string concat") {
    spec(""" "a" + "b" """) shouldBe StringConstant("ab")
  }

  property("ExtractCreationInfo") {
    spec("SELF.creationInfo") shouldBe ExtractCreationInfo(Self)
    spec("SELF.creationInfo._1") shouldBe SelectField(ExtractCreationInfo(Self), 1)
    spec("SELF.creationInfo._2") shouldBe SelectField(ExtractCreationInfo(Self), 2)
  }
}
