package org.ergoplatform

import java.math.BigInteger

import org.ergoplatform.ErgoAddressEncoder.{hash256, MainnetNetworkPrefix, TestnetNetworkPrefix}
import org.ergoplatform.SigmaConstants.ScriptCostLimit
import org.ergoplatform.validation.{ValidationException, ValidationRules}
import org.scalatest.{Assertion, TryValues}
import sigmastate.basics.DLogProtocol
import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer
import sigmastate.serialization.ValueSerializer
import scorex.util.encode.Base58
import sigmastate.{SigmaAnd, SType, CrossVersionProps}
import sigmastate.Values.{UnparsedErgoTree, Constant, EvaluatedValue, ByteArrayConstant, IntConstant, ErgoTree}
import sigmastate.eval.IRContext
import sigmastate.helpers._
import sigmastate.helpers.TestingHelpers._
import sigmastate.interpreter.{ContextExtension, CostedProverResult}
import sigmastate.interpreter.Interpreter.{ScriptNameProp, ScriptEnv}
import sigmastate.lang.Terms.ValueOps
import sigmastate.lang.exceptions.{CosterException, CostLimitException}
import sigmastate.utils.Helpers._
import special.sigma.SigmaDslTesting

class ErgoAddressSpecification extends SigmaDslTesting
  with TryValues with CrossVersionProps {

  private implicit val ergoAddressEncoder: ErgoAddressEncoder =
    new ErgoAddressEncoder(TestnetNetworkPrefix)

  def addressRoundtrip(addr: ErgoAddress): Assertion = {
    val addrStr = ergoAddressEncoder.toString(addr)
    val parsedAddr = ergoAddressEncoder.fromString(addrStr).get
    parsedAddr shouldBe addr
    ergoAddressEncoder.toString(parsedAddr) shouldBe addrStr
  }

  property("P2PK roundtrip") {
    forAll(proveDlogGen) { pk =>
      addressRoundtrip(P2PKAddress(pk))
    }
  }

  property("SHA roundtrip") {
    forAll(proveDlogGen) { pk =>
      addressRoundtrip(Pay2SHAddress(ErgoTree.fromSigmaBoolean(ergoTreeHeaderInTests, pk)))
    }
  }

  property("SA roundtrip") {
    forAll(proveDlogGen) { pk =>
      addressRoundtrip(Pay2SAddress(ErgoTree.fromSigmaBoolean(ergoTreeHeaderInTests, pk)))
    }
  }

  property("P2SH proper bytes to track") {
    forAll(proveDlogGen) { pk =>
      val p2sh = Pay2SHAddress(ErgoTree.fromSigmaBoolean(ergoTreeHeaderInTests, pk))

      //search we're doing to find a box potentially corresponding to some address
      DefaultSerializer.serializeErgoTree(p2sh.script).containsSlice(p2sh.contentBytes) shouldBe true
    }
  }

  property("P2S proper bytes to track") {
    forAll(proveDlogGen) { pk =>
      val p2s = Pay2SAddress(ErgoTree.fromSigmaBoolean(ergoTreeHeaderInTests, pk))

      //search we're doing to find a box potentially corresponding to some address
      DefaultSerializer.serializeErgoTree(p2s.script).containsSlice(p2s.contentBytes) shouldBe true
    }
  }

  def testFromProposition(scriptVersion: Byte,
                          expectedP2S: String, expectedP2SH: String, expectedP2PK: String) = {
    val pk: DLogProtocol.ProveDlog = DLogProverInput(BigInteger.ONE).publicImage
    val pk10: DLogProtocol.ProveDlog = DLogProverInput(BigInteger.TEN).publicImage

    val p2s: Pay2SAddress = Pay2SAddress(
      ErgoTree.fromProposition(
        ErgoTree.headerWithVersion(scriptVersion),
        SigmaAnd(pk, pk10)))
    val p2sh: Pay2SHAddress = Pay2SHAddress(pk)
    val p2pk: P2PKAddress = P2PKAddress(pk)

    p2s.toString shouldBe expectedP2S
    p2sh.toString shouldBe expectedP2SH
    p2pk.toString shouldBe expectedP2PK

    assertResult(true)(p2s != p2sh && p2sh != p2s)
    assertResult(true)(p2sh != p2pk && p2pk != p2sh)
    assertResult(true)(p2pk != p2s && p2s != p2pk)

    val parsed_p2s = ergoAddressEncoder.fromProposition(p2s.script).success.value
    assertResult(false)(parsed_p2s eq p2s)
    assertResult(true)(parsed_p2s == p2s && p2s == parsed_p2s)
    parsed_p2s.hashCode() shouldBe p2s.hashCode()

    val parsed_p2sh = ergoAddressEncoder.fromProposition(p2sh.script).success.value
    assertResult(false)(parsed_p2sh eq p2sh)
    assertResult(true)(parsed_p2sh == p2sh && p2sh == parsed_p2sh)
    parsed_p2sh.hashCode() shouldBe p2sh.hashCode()

    val parsed_p2pk = ergoAddressEncoder.fromProposition(p2pk.script).success.value
    assertResult(false)(parsed_p2pk eq p2pk)
    assertResult(true)(parsed_p2pk == p2pk && p2pk == parsed_p2pk)
    parsed_p2pk.hashCode() shouldBe p2pk.hashCode()

    val tree = mkTestErgoTree(pk)
    val p2s_2: Pay2SAddress = Pay2SAddress(tree) // address created via P2S constructor method
    assertResult(true)(p2s_2 != p2pk && p2pk != p2s_2)

    // not equal to created via encoder (how "good" is that!)
    val from_tree = ergoAddressEncoder.fromProposition(tree).success.value
    assertResult(true)(from_tree != p2s_2)
    assertResult(true)(from_tree == p2pk)
  }

  property("fromProposition() should properly distinct all types of addresses from script AST") {
    testFromProposition(scriptVersion = 0,
      expectedP2S = "JryiCXrZf5VDetH1PM7rKDX3q4sLF34AdErWJFqG87Hf5ikTDf636b35Nr7goWMdRUKA3ZPxdeqFNbQzBjhnDR9SUMYwDX1tdV8ZuGgXwQPaRVcB9",
      expectedP2SH = "qETVgcEctaXurNbFRgGUcZEGg4EKa8R4a5UNHY7",
      expectedP2PK = "3WwXpssaZwcNzaGMv3AgxBdTPJQBt5gCmqBsg3DykQ39bYdhJBsN")

    testFromProposition(scriptVersion = 1,
      expectedP2S = "2MzJLjzX6UNfJHSVvioB6seYZ99FpWHB4Ds1gekHPv5KtNmLJUecgRWwvcGEqbt8ZAokUxGvKMuNgMZFzkPPdTGiYzPQoSR55NT5isCidMywgp52LYV",
      expectedP2SH = "qETVgcEctaXurNbFRgGUcZEGg4EKa8R4a5UNHY7",
      expectedP2PK = "3WwXpssaZwcNzaGMv3AgxBdTPJQBt5gCmqBsg3DykQ39bYdhJBsN")
  }

  property("decode with wrong address") {
    assertExceptionThrown(
      ergoAddressEncoder.fromString(
        "JryiCXrZf5VDetH1PM7rKDX3q4sLF34AdErWJFqG87Hf5ikTDf636b35Nr7goWMdRUKA3ZPxdeqFNbQzBjhnDR9SUMYwDX1tdV8ZuGgXwQPaRVcB8")
          .getOrThrow,
      t => t.getMessage.contains("Checksum check fails")
    )
    assertExceptionThrown(
      ergoAddressEncoder.fromString(
        "2MzJLjzX6UNfJHSVvioB6seYZ99FpWHB4Ds1gekHPv5KtNmLJUecgRWwvcGEqbt8ZAokUxGvKMuNgMZFzkPPdTGiYzPQoSR55NT5isCidMywgp52LYU")
          .getOrThrow,
      t => t.getMessage.contains("Checksum check fails")
    )

    {
      val invalid_p2sh = new Pay2SHAddress(Array[Byte](1, 2, 3))
      val addrStr = ergoAddressEncoder.toString(invalid_p2sh)

      assertExceptionThrown(
        ergoAddressEncoder.fromString(addrStr).getOrThrow,
        t => t.getMessage.contains("Improper content in P2SH script: 41fKjb7zWNw")
      )
    }

    {
      val pk: DLogProtocol.ProveDlog = DLogProverInput(BigInteger.ONE).publicImage
      val p2pk = P2PKAddress(pk)(ergoAddressEncoder)

      val invalidAddrType = 4.toByte
      val withNetworkByte = (ergoAddressEncoder.networkPrefix + invalidAddrType).toByte +: p2pk.contentBytes

      val checksum = hash256(withNetworkByte).take(ErgoAddressEncoder.ChecksumLength)
      val invalidAddrStr = Base58.encode(withNetworkByte ++ checksum)

      assertExceptionThrown(
        ergoAddressEncoder.fromString(invalidAddrStr).getOrThrow,
        t => t.getMessage.contains("Unsupported address type: 4")
      )
    }

    {
      val unparsedTree = new ErgoTree(
        (ErgoTree.ConstantSegregationHeader | ergoTreeHeaderInTests).toByte,
        Array[Constant[SType]](),
        Left(UnparsedErgoTree(Array[Byte](), ValidationException("", ValidationRules.CheckTypeCode, Seq())))
      )
      assertExceptionThrown(
        ergoAddressEncoder.fromProposition(unparsedTree).getOrThrow,
        t => t.getMessage.contains("Cannot create ErgoAddress form unparsed ergo tree")
      )
    }

    {
      val invalidTree = new ErgoTree(
        (ErgoTree.ConstantSegregationHeader | ergoTreeHeaderInTests).toByte,
        Array[Constant[SType]](),
        Right(IntConstant(10).asSigmaProp)
      )
      assertExceptionThrown(
        ergoAddressEncoder.fromProposition(invalidTree).getOrThrow,
        t => t.getMessage.contains("Cannot create ErgoAddress form proposition")
      )
    }
  }

  property("decode with wrong network prefix") {
    forAll(proveDlogGen) { pk =>
      val mainnetEncoder = new ErgoAddressEncoder(MainnetNetworkPrefix)
      val testnetEncoder = new ErgoAddressEncoder(TestnetNetworkPrefix)
      val mnAddr = P2PKAddress(pk)(mainnetEncoder)
      val tnAddr = P2PKAddress(pk)(testnetEncoder)

      an[RuntimeException] should be thrownBy mainnetEncoder.fromString(tnAddr.toString).get
      an[RuntimeException] should be thrownBy testnetEncoder.fromString(mnAddr.toString).get
    }
  }

  def testPay2SHAddress(address: Pay2SHAddress, scriptBytes: Array[Byte])(implicit IR: IRContext) = {
    val scriptId = 1.toByte
    val boxToSpend = testBox(10, address.script, creationHeight = 5)
    val ctx = ErgoLikeContextTesting.dummy(boxToSpend, activatedVersionInTests)
        .withExtension(ContextExtension(Seq(
          scriptId -> ByteArrayConstant(scriptBytes)  // provide script bytes in context variable
        ).toMap))

    val env: ScriptEnv = Map()
    val prover = new ErgoLikeTestProvingInterpreter()
    val pr = prover.prove(env + (ScriptNameProp -> s"prove"), address.script, ctx, fakeMessage).getOrThrow

    val verifier = new ErgoLikeTestInterpreter
    verifier.verify(env + (ScriptNameProp -> s"verify_ext"), address.script, ctx, pr.proof, fakeMessage).getOrThrow._1 shouldBe true
  }

  property("spending a box protected by P2SH contract") {
    implicit lazy val IR = new TestingIRContext

    val script = "{ 1 < 2 }"
    val prop = compile(Map.empty, script).asBoolValue.toSigmaProp
    val scriptBytes = ValueSerializer.serialize(prop)

    testPay2SHAddress(Pay2SHAddress(prop), scriptBytes)

    val tree = mkTestErgoTree(prop)
    testPay2SHAddress(Pay2SHAddress(tree), scriptBytes) // NOTE: same scriptBytes regardless of ErgoTree version
  }

  property("negative cases: deserialized script + costing exceptions") {
   implicit lazy val IR = new TestingIRContext

    def testPay2SHAddress(address: Pay2SHAddress, script: (Byte, EvaluatedValue[_ <: SType]), costLimit: Int = ScriptCostLimit.value): CostedProverResult = {
      val boxToSpend = testBox(10, address.script, creationHeight = 5)
      val ctx = copyContext(ErgoLikeContextTesting.dummy(boxToSpend, activatedVersionInTests)
          .withExtension(ContextExtension(Seq(
            script // provide script bytes in context variable
          ).toMap)))(costLimit = costLimit)

      val prover = new ErgoLikeTestProvingInterpreter()
      prover.prove(address.script, ctx, fakeMessage).getOrThrow
    }

    val scriptVarId = 1.toByte
    val script = "{ 1 < 2 }"
    val prop = compile(Map.empty, script).asBoolValue.toSigmaProp
    val scriptBytes = ValueSerializer.serialize(prop)
    val addr = Pay2SHAddress(prop)

    // when everything is ok
    testPay2SHAddress(addr, script = scriptVarId -> ByteArrayConstant(scriptBytes))

    // when limit is low
    {
      // choose limit less than total cost:
      // totalCost(2671) = addr.script.complexity(2277) + prop complexity(164) + scaledCost(230)
      val deliberatelySmallLimit = 2600

      assertExceptionThrown(
      {
        testPay2SHAddress(addr,
          script = scriptVarId -> ByteArrayConstant(scriptBytes),
          costLimit = deliberatelySmallLimit)
      },
      { t =>
        t.isInstanceOf[CostLimitException] &&
            t.getMessage.contains(
              s"Estimated execution cost 2671 exceeds the limit $deliberatelySmallLimit")}
      )
    }

    // when limit is low
    {
      // choose limit less than addr.script.complexity == 2277 + script complexity == 164
      val deliberatelySmallLimit = 2300

      assertExceptionThrown(
      {
        testPay2SHAddress(addr,
          script = scriptVarId -> ByteArrayConstant(scriptBytes),
          costLimit = deliberatelySmallLimit)
      },
      { t =>
        t.isInstanceOf[CostLimitException] &&
            t.getMessage.contains(
              s"Estimated execution cost 2441 exceeds the limit $deliberatelySmallLimit")}
      )
    }

    // when limit is even lower than tree complexity
    {
      // choose limit less than addr.script.complexity == 2277
      val deliberatelySmallLimit = 2000

      assertExceptionThrown(
      {
        testPay2SHAddress(addr,
          script = scriptVarId -> ByteArrayConstant(scriptBytes),
          costLimit = deliberatelySmallLimit)
      },
      { t =>
        t.isInstanceOf[CostLimitException] &&
            t.getMessage.contains(
              s"Estimated execution cost 2277 exceeds the limit $deliberatelySmallLimit")}
      )
    }

    // when script var have invalid type
    assertExceptionThrown(
      testPay2SHAddress(addr, script = scriptVarId -> IntConstant(10)),
      { t =>
        t.isInstanceOf[CosterException] &&
        t.getMessage.contains(s"Don't know how to evalNode(DeserializeContext(")}
    )

    // when script var have invalid id
    val invalidId: Byte = 2
    assertExceptionThrown(
      testPay2SHAddress(addr, script = invalidId -> IntConstant(10)),
      { t =>
        t.isInstanceOf[CosterException] &&
        t.getMessage.contains(s"Don't know how to evalNode(DeserializeContext(")}
    )
  }

}