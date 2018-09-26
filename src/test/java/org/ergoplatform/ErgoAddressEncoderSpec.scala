package org.ergoplatform

import java.math.BigInteger

import org.scalatest.{Matchers, PropSpec, TryValues}
import scapi.sigma.DLogProtocol
import scapi.sigma.DLogProtocol.DLogProverInput
import sigmastate.Values
import sigmastate.serialization.ValueSerializer

class ErgoAddressEncoderSpec extends PropSpec with Matchers with TryValues {

  property("scriptToAddress() should properly distinct all types of addresses from plain script bytes") {

    implicit val encoder: ErgoAddressEncoder = ErgoAddressEncoder(networkPrefix = 0x10)

    val pk: DLogProtocol.ProveDlog = DLogProverInput(BigInteger.ONE).publicImage
    val sh: Array[Byte] = ErgoAddressEncoder.hash192(ValueSerializer.serialize(pk))

    val p2s: Pay2SAddress = Pay2SAddress(Values.TrueLeaf)
    val p2sh: Pay2SHAddress = new Pay2SHAddress(sh)
    val p2pk: P2PKAddress = P2PKAddress(pk)

    encoder.fromProposition(p2s.script).success.value.isInstanceOf[Pay2SAddress] shouldBe true
    encoder.fromProposition(p2sh.script).success.value.isInstanceOf[Pay2SHAddress] shouldBe true
    encoder.fromProposition(p2pk.script).success.value.isInstanceOf[P2PKAddress] shouldBe true
  }
}
