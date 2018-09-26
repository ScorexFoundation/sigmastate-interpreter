package org.ergoplatform

import org.ergoplatform.ErgoLikeContext.Metadata
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Assertion, Matchers, PropSpec}
import sigmastate.serialization.ValueSerializer
import sigmastate.serialization.generators.ValueGenerators

class ErgoAddressSpecification extends PropSpec
  with ValueGenerators
  with PropertyChecks
  with Matchers {

  private implicit val ergoAddressEncoder = new ErgoAddressEncoder(Metadata.TestnetNetworkPrefix)

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
      ValueSerializer.serialize(p2sh.script).containsSlice(p2sh.contentBytes) shouldBe true
    }
  }

  property("P2S proper bytes to track") {
    forAll(proveDlogGen) { s =>
      val p2s = Pay2SAddress(s)

      //search we're doing to find a box potentially corresponding to some address
      ValueSerializer.serialize(p2s.script).containsSlice(p2s.contentBytes) shouldBe true
    }
  }
}


