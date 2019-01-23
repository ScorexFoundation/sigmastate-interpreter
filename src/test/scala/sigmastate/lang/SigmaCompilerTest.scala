package sigmastate.lang

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import org.ergoplatform.{ErgoAddressEncoder, Height, P2PKAddress}
import org.scalatest.exceptions.TestFailedException
import scorex.util.encode.Base58
import sigmastate.Values._
import sigmastate._
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.Terms.{Apply, ZKProofBlock}
import sigmastate.lang.exceptions.{CosterException, InvalidArguments, TyperException}
import sigmastate.lang.syntax.ParserException
import sigmastate.serialization.ValueSerializer
import sigmastate.serialization.generators.ValueGenerators
import sigmastate.utxo.{ByIndex, GetVar}

class SigmaCompilerTest extends SigmaTestingCommons with LangTests with ValueGenerators {
  import CheckingSigmaBuilder._
  implicit lazy val IR = new TestingIRContext

  private def comp(env: ScriptEnv, x: String): Value[SType] = compileWithCosting(env, x)
  private def comp(x: String): Value[SType] = compileWithCosting(env, x)
  private def compWOCosting(x: String): Value[SType] = compile(env, x)
  private def compWOCosting(env: ScriptEnv, x: String): Value[SType] = compile(env, x)

  private def fail(env: ScriptEnv, x: String, index: Int, expected: Any): Unit = {
    try {
      val res = compiler.compile(env, x)
      assert(false, s"Error expected")
    } catch {
      case e: TestFailedException =>
        throw e
      case pe: ParserException if pe.parseError.isDefined =>
        val p = pe
        val i = pe.parseError.get.index
        val l = pe.parseError.get.lastParser
        i shouldBe index
        l.toString shouldBe expected.toString
    }
  }

  private def testMissingCosting(script: String, expected: SValue): Unit = {
    compWOCosting(script) shouldBe expected
    // when implemented in coster this should be changed to a positive expectation
    an [CosterException] should be thrownBy comp(env, script)
  }

  property("array indexed access") {
    comp(env, "Coll(1)(0)") shouldBe
      ByIndex(ConcreteCollection(IndexedSeq(IntConstant(1)))(SInt), 0)
    comp(env, "Coll(Coll(1))(0)(0)") shouldBe
        ByIndex(ByIndex(ConcreteCollection(IndexedSeq(ConcreteCollection(IndexedSeq(IntConstant(1)))))(SCollection(SInt)), 0), 0)
    comp(env, "arr1(0)") shouldBe ByIndex(ByteArrayConstant(Array(1, 2)), 0)
  }

  property("array indexed access with default value") {
    comp(env, "Coll(1).getOrElse(0, 1)") shouldBe
      ByIndex(ConcreteCollection(IndexedSeq(IntConstant(1)))(SInt), 0, Some(IntConstant(1)))
    comp(env, "Coll(Coll(1)).getOrElse(0, Coll(2))(0)") shouldBe
      ByIndex(
        ByIndex(
          ConcreteCollection(IndexedSeq(ConcreteCollection(IndexedSeq(IntConstant(1)))))(SCollection(SInt)),
          0,
          Some(ConcreteCollection(Vector(IntConstant(2))))),
        0)
    comp(env, "arr1.getOrElse(999, 0.toByte)") shouldBe
      ByIndex(ByteArrayConstant(Array(1, 2)), IntConstant(999), Some(ByteConstant(0)))
  }

  property("predefined functions") {
    comp(env, "anyOf(Coll(c1, c2))") shouldBe OR(ConcreteCollection(Vector(TrueLeaf, FalseLeaf)))
    comp(env, "blake2b256(getVar[Coll[Byte]](10).get)") shouldBe CalcBlake2b256(GetVarByteArray(10).get)
    comp(env, "sha256(getVar[Coll[Byte]](10).get)") shouldBe CalcSha256(GetVarByteArray(10).get)
    comp(env, "10.toByte") shouldBe ByteConstant(10)
    comp(env, "Coll(1)(0).toByte") shouldBe
      Downcast(ByIndex(ConcreteCollection(Vector(IntConstant(1)),SInt),IntConstant(0),None), SByte)
    comp(env, "allOf(Coll(c1, c2))") shouldBe AND(ConcreteCollection(Vector(TrueLeaf, FalseLeaf)))
    comp(env, "getVar[Byte](10).get") shouldBe GetVarByte(10).get
    comp(env, "getVar[Coll[Byte]](10).get") shouldBe GetVarByteArray(10).get
  }

  property("user-defined functions") {
    comp("{ def f(i: Int) = { i + 1 }; f(2) }") shouldBe Apply(
      FuncValue(Vector((1,SInt)),Plus(ValUse(1,SInt), IntConstant(1))),
      Vector(IntConstant(2)))
  }

  property("negative tests") {
    fail(env, "(10", 3, "\")\"")
    fail(env, "10)", 2, "End")
    fail(env, "X)", 1, "End")
    fail(env, "(X", 2, "\")\"")
    fail(env, "{ X", 3, "\"}\"")
    fail(env, "{ val X", 7, "\"=\"")
  }

  property("allOf") {
    comp("allOf(Coll[Boolean](true, false))") shouldBe AND(TrueLeaf, FalseLeaf)
  }

  property("anyOf") {
    comp("anyOf(Coll[Boolean](true, false))") shouldBe OR(TrueLeaf, FalseLeaf)
  }

  property("atLeast") {
    comp("atLeast(2, Coll[SigmaProp](p1, p2))") shouldBe AtLeast(2, p1, p2)
  }

  property("ZKProof") {
    testMissingCosting("ZKProof { sigmaProp(HEIGHT > 1000) }",
      ZKProofBlock(BoolToSigmaProp(GT(Height, IntConstant(1000)))))
  }

  property("sigmaProp") {
    comp("sigmaProp(HEIGHT > 1000)") shouldBe BoolToSigmaProp(GT(Height, IntConstant(1000)))
  }

  property("getVar") {
    comp("getVar[Byte](10).get") shouldBe GetVar(10.toByte, SByte).get
    comp("getVar[Byte](10L).get") shouldBe GetVar(10.toByte, SByte).get
    an[TyperException] should be thrownBy comp("getVar[Byte](\"ha\")")
  }

  property("PK (testnet network prefix)") {
    implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(TestnetNetworkPrefix)
    val dk1 = proveDlogGen.sample.get
    val encodedP2PK = P2PKAddress(dk1).toString
    val code = s"""PK("$encodedP2PK")"""
    comp(code) shouldEqual SigmaPropConstant(dk1)
  }

  property("fromBaseX") {
    comp(""" fromBase58("r") """) shouldBe ByteArrayConstant(Array(49))
    comp(""" fromBase64("MQ") """) shouldBe ByteArrayConstant(Array(49))
    comp(""" fromBase64("M" + "Q") """) shouldBe ByteArrayConstant(Array(49))
  }

  property("deserialize") {
    def roundtrip[T <: SType](c: EvaluatedValue[T], typeSig: String) = {
      val bytes = ValueSerializer.serialize(c)
      val str = Base58.encode(bytes)
      comp(env, s"deserialize[$typeSig](" + "\"" + str + "\")") shouldBe c
    }
    roundtrip(ByteArrayConstant(Array[Byte](2)), "Coll[Byte]")
    roundtrip(Tuple(ByteArrayConstant(Array[Byte](2)), LongConstant(4)), "(Coll[Byte], Long)")
    an[InvalidArguments] should be thrownBy roundtrip(ByteArrayConstant(Array[Byte](2)), "Coll[Long]")
  }

  property("deserialize fails") {
    // more then one argument
    an[TyperException] should be thrownBy comp(env, """deserialize[Int]("test", "extra argument")""")
    // invalid chat in Base58 string
    an[AssertionError] should be thrownBy comp(env, """deserialize[Int]("0")""")
    // more than one type
    an[TyperException] should be thrownBy comp(env, """deserialize[Int, Byte]("test")""")
    // not a string constant
    an[TyperException] should be thrownBy comp(env, """deserialize[Int](1)""")
  }

  property("longToByteArray") {
    comp("longToByteArray(1L)") shouldBe LongToByteArray(LongConstant(1))
  }

  property("byteArrayToBigInt") {
    comp("byteArrayToBigInt(longToByteArray(1L))") shouldBe ByteArrayToBigInt(LongToByteArray(LongConstant(1)))
  }

  property("failed fromBaseX (invalid input)") {
    an[AssertionError] should be thrownBy comp(""" fromBase58("^%$#@")""")
    an[IllegalArgumentException] should be thrownBy comp(""" fromBase64("^%$#@")""")
  }

  property("decodePoint") {
    comp(env, "decodePoint(Coll[Byte](1.toByte))") shouldBe DecodePoint(ConcreteCollection(ByteConstant(1)))
  }

  property("logicalNot") {
    testMissingCosting("!true", LogicalNot(TrueLeaf))
  }

  property("Negation") {
    testMissingCosting("-HEIGHT", Negation(Height))
  }

  property("BitInversion") {
    testMissingCosting("~1", BitInversion(IntConstant(1)))
  }

  property("LogicalXor") {
    testMissingCosting("true ^ false", BinXor(TrueLeaf, FalseLeaf))
  }

  property("BitwiseOr") {
    testMissingCosting("1 | 2", mkBitOr(IntConstant(1), IntConstant(2)))
  }

  property("BitwiseAnd") {
    testMissingCosting("1 & 2", mkBitAnd(IntConstant(1), IntConstant(2)))
  }

  property("BitwiseXor") {
    testMissingCosting("1 ^ 2", mkBitXor(IntConstant(1), IntConstant(2)))
  }

  property("BitShiftRight") {
    testMissingCosting("1 >> 2", mkBitShiftRight(IntConstant(1), IntConstant(2)))
  }

  property("BitShiftLeft") {
    testMissingCosting("1 << 2", mkBitShiftLeft(IntConstant(1), IntConstant(2)))
  }

  property("BitShiftRightZeroed") {
    testMissingCosting("1 >>> 2", mkBitShiftRightZeroed(IntConstant(1), IntConstant(2)))
  }

}
