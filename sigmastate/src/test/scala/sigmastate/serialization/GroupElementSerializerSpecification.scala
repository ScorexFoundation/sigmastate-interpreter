package sigmastate.serialization

import sigmastate.crypto.CryptoFacade
import sigmastate.interpreter.CryptoConstants
import sigmastate.eval._

class GroupElementSerializerSpecification extends SerializationSpecification {

  property("Identity point serialization") {
    val group = CryptoConstants.dlogGroup
    val identity = group.identity

    val bytes = GroupElementSerializer.toBytes(identity)
    bytes.length shouldBe CryptoConstants.EncodedGroupElementLength
    bytes.forall(_ == 0) shouldBe true
    GroupElementSerializer.parse(SigmaSerializer.startReader(bytes, 0)).isIdentity shouldBe true
  }

  property("point roundtrip") {
    forAll(groupElementConstGen){ge =>
      val bytes = GroupElementSerializer.toBytes(ge.value)
      bytes.length shouldBe CryptoConstants.EncodedGroupElementLength
      val restored = GroupElementSerializer.parse(SigmaSerializer.startReader(bytes, 0))
      CryptoFacade.getAffineXCoord(CryptoFacade.normalizePoint(restored)) shouldBe
        CryptoFacade.getAffineXCoord(CryptoFacade.normalizePoint(ge.value))
      CryptoFacade.getAffineYCoord(CryptoFacade.normalizePoint(restored)) shouldBe
        CryptoFacade.getAffineYCoord(CryptoFacade.normalizePoint(ge.value))
    }
  }
}
