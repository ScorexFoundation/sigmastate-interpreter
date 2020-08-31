package org.ergoplatform

import java.math.BigInteger

import org.ergoplatform.ErgoAddressEncoder.{hash256, MainnetNetworkPrefix, TestnetNetworkPrefix}
import org.ergoplatform.validation.{ValidationException, ValidationRules}
import org.scalatest.{Assertion, TryValues}
import sigmastate.basics.DLogProtocol
import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer
import sigmastate.serialization.ValueSerializer
import scorex.util.encode.Base58
import sigmastate.{SigmaAnd, SType}
import sigmastate.Values.{UnparsedErgoTree, Constant, ByteArrayConstant, IntConstant, ErgoTree}
import sigmastate.eval.IRContext
import sigmastate.helpers._
import sigmastate.interpreter.ContextExtension
import sigmastate.interpreter.Interpreter.{ScriptNameProp, ScriptEnv}
import sigmastate.lang.Terms.ValueOps
import sigmastate.utils.Helpers._
import special.sigma.SigmaDslTesting

class ErgoAddressSpecification extends SigmaDslTesting with TryValues {

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
      addressRoundtrip(Pay2SHAddress(ErgoTree.fromSigmaBoolean(pk)))
    }
  }

  property("SA roundtrip") {
    forAll(proveDlogGen) { pk =>
      addressRoundtrip(Pay2SAddress(ErgoTree.fromSigmaBoolean(pk)))
    }
  }

  property("P2SH proper bytes to track") {
    forAll(proveDlogGen) { pk =>
      val p2sh = Pay2SHAddress(ErgoTree.fromSigmaBoolean(pk))

      //search we're doing to find a box potentially corresponding to some address
      DefaultSerializer.serializeErgoTree(p2sh.script).containsSlice(p2sh.contentBytes) shouldBe true
    }
  }

  property("P2S proper bytes to track") {
    forAll(proveDlogGen) { pk =>
      val p2s = Pay2SAddress(ErgoTree.fromSigmaBoolean(pk))

      //search we're doing to find a box potentially corresponding to some address
      DefaultSerializer.serializeErgoTree(p2s.script).containsSlice(p2s.contentBytes) shouldBe true
    }
  }

  property("fromProposition() should properly distinct all types of addresses from script AST") {
    val pk: DLogProtocol.ProveDlog = DLogProverInput(BigInteger.ONE).publicImage
    val pk10: DLogProtocol.ProveDlog = DLogProverInput(BigInteger.TEN).publicImage

    val p2s: Pay2SAddress = Pay2SAddress(ErgoTree.fromProposition(SigmaAnd(pk, pk10)))
    val p2sh: Pay2SHAddress = Pay2SHAddress(pk)
    val p2pk: P2PKAddress = P2PKAddress(pk)

    p2s.toString shouldBe "JryiCXrZf5VDetH1PM7rKDX3q4sLF34AdErWJFqG87Hf5ikTDf636b35Nr7goWMdRUKA3ZPxdeqFNbQzBjhnDR9SUMYwDX1tdV8ZuGgXwQPaRVcB9"
    p2sh.toString shouldBe "qETVgcEctaXurNbFRgGUcZEGg4EKa8R4a5UNHY7"
    p2pk.toString shouldBe "3WwXpssaZwcNzaGMv3AgxBdTPJQBt5gCmqBsg3DykQ39bYdhJBsN"

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

    val tree = ErgoTree.fromProposition(pk)
    val p2s_2: Pay2SAddress = Pay2SAddress(tree) // address created via P2S constructor method
    assertResult(true)(p2s_2 != p2pk && p2pk != p2s_2)

    // not equal to created via encoder (how "good" is that!)
    val from_tree = ergoAddressEncoder.fromProposition(tree).success.value
    assertResult(true)(from_tree != p2s_2)
    assertResult(true)(from_tree == p2pk)
  }

  property("decode with wrong address") {
    assertExceptionThrown(
      ergoAddressEncoder.fromString(
        "JryiCXrZf5VDetH1PM7rKDX3q4sLF34AdErWJFqG87Hf5ikTDf636b35Nr7goWMdRUKA3ZPxdeqFNbQzBjhnDR9SUMYwDX1tdV8ZuGgXwQPaRVcB8")
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
        ErgoTree.ConstantSegregationHeader,
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
        ErgoTree.ConstantSegregationHeader,
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
    val boxToSpend = ErgoBox(10, address.script, creationHeight = 5)
    val ctx = ErgoLikeContextTesting.dummy(boxToSpend)
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

    val tree = ErgoTree.fromProposition(prop)
    testPay2SHAddress(Pay2SHAddress(tree), scriptBytes)
  }
}