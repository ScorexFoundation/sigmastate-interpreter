package org.ergoplatform

import java.math.BigInteger

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Assertion, Matchers, PropSpec, TryValues}
import scapi.sigma.DLogProtocol
import scapi.sigma.DLogProtocol.DLogProverInput
import sigmastate.Values
import sigmastate.serialization.{ErgoTreeSerializer, ValueSerializer}
import sigmastate.serialization.generators.ValueGenerators

class ErgoAddressSpecification extends PropSpec
  with ValueGenerators
  with PropertyChecks
  with Matchers
  with TryValues {

  private implicit val ergoAddressEncoder: ErgoAddressEncoder =
    new ErgoAddressEncoder(ErgoAddressEncoder.TestnetNetworkPrefix)

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
      addressRoundtrip(Pay2SHAddress(pk))
    }
  }

  property("SA roundtrip") {
    forAll(proveDlogGen) { pk =>
      addressRoundtrip(Pay2SAddress(pk))
    }
  }

  property("P2SH proper bytes to track") {
    forAll(proveDlogGen) { s =>
      val p2sh = Pay2SHAddress(s)

      //search we're doing to find a box potentially corresponding to some address
      ErgoTreeSerializer.DefaultSerializer.serializeWithSegregation(p2sh.script).containsSlice(p2sh.contentBytes) shouldBe true
    }
  

  property("P2S proper bytes to track") {
    forAll(proveDlogGen) { s =>
      val p2s = Pay2SAddress(s)

      //search we're doing to find a box potentially corresponding to some address
      ErgoTreeSerializer.serialize(p2s.script).containsSlice(p2s.contentBytes) shouldBe true
    }
  }

  property("fromProposition() should properly distinct all types of addresses from script AST") {

    val pk: DLogProtocol.ProveDlog = DLogProverInput(BigInteger.ONE).publicImage
    val sh: Array[Byte] = ErgoAddressEncoder.hash192(ValueSerializer.serialize(pk))

    val p2s: Pay2SAddress = Pay2SAddress(Values.TrueLeaf)
    val p2sh: Pay2SHAddress = new Pay2SHAddress(sh)
    val p2pk: P2PKAddress = P2PKAddress(pk)

    ergoAddressEncoder.fromProposition(p2s.script).success.value.isInstanceOf[Pay2SAddress] shouldBe true
    ergoAddressEncoder.fromProposition(p2sh.script).success.value.isInstanceOf[Pay2SHAddress] shouldBe true
    ergoAddressEncoder.fromProposition(p2pk.script).success.value.isInstanceOf[P2PKAddress] shouldBe true
  }
}