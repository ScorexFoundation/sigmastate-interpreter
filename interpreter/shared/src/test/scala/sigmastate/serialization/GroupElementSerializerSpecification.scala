package sigmastate.serialization

import sigmastate.crypto.CryptoConstants
import sigmastate.crypto.CryptoFacade
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
      CryptoFacade.getAffineXCoord(CryptoFacade.normalize(restored)) shouldBe
        CryptoFacade.getAffineXCoord(CryptoFacade.normalize(ge.value))
      CryptoFacade.getAffineYCoord(CryptoFacade.normalize(restored)) shouldBe
        CryptoFacade.getAffineYCoord(CryptoFacade.normalize(ge.value))
    }
  }
}
