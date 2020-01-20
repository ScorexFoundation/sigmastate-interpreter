package org.ergoplatform

import java.math.BigInteger

import org.ergoplatform.ErgoAddressEncoder.{MainnetNetworkPrefix, TestnetNetworkPrefix}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{PropSpec, Assertion, Matchers, TryValues}
import sigmastate.basics.DLogProtocol
import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer
import sigmastate.serialization.ValueSerializer
import sigmastate.serialization.generators.ObjectGenerators
import org.ergoplatform.ErgoScriptPredef._
import org.ergoplatform.validation.ValidationSpecification
import sigmastate.{AvlTreeData, SType}
import sigmastate.Values.{EvaluatedValue, SigmaPropConstant, ByteArrayConstant, IntConstant, ErgoTree}
import sigmastate.eval.IRContext
import sigmastate.helpers._
import sigmastate.interpreter.{ContextExtension, Interpreter}
import sigmastate.interpreter.Interpreter.{ScriptNameProp, ScriptEnv}
import sigmastate.lang.Terms.ValueOps
import sigmastate.utils.Helpers._

class ErgoAddressSpecification extends PropSpec
  with ObjectGenerators
  with PropertyChecks
  with Matchers
  with TryValues
  with SigmaTestingCommons {
  private implicit val ergoAddressEncoder: ErgoAddressEncoder =
    new ErgoAddressEncoder(TestnetNetworkPrefix)

  def addressRoundtrip(addr: ErgoAddress): Assertion = {
    ergoAddressEncoder.fromString(ergoAddressEncoder.toString(addr)).get shouldBe addr
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

    val p2s: Pay2SAddress = Pay2SAddress(TrueProp)
    val p2sh: Pay2SHAddress = Pay2SHAddress(pk)
    val p2pk: P2PKAddress = P2PKAddress(pk)

    ergoAddressEncoder.fromProposition(p2s.script).success.value shouldBe p2s
    ergoAddressEncoder.fromProposition(p2sh.script).success.value shouldBe p2sh
    ergoAddressEncoder.fromProposition(p2pk.script).success.value.isInstanceOf[P2PKAddress] shouldBe true
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