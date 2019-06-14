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
import sigmastate.Values.ErgoTree

class ErgoAddressSpecification extends PropSpec
  with ObjectGenerators
  with PropertyChecks
  with Matchers
  with TryValues
  with ValidationSpecification {
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

}