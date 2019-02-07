package sigmastate.serialization

import sigmastate.AvlTreeFlags

class AvlTreeFlagsSpecification extends SerializationSpecification {

  property("roundtrip for all the AVL tree flags"){
    val u1 = AvlTreeFlags(false, false, false)
    val u2 = AvlTreeFlags(false, false, true)
    val u3 = AvlTreeFlags(false, true, false)
    val u4 = AvlTreeFlags(false, true, true)
    val u5 = AvlTreeFlags(true, false, false)
    val u6 = AvlTreeFlags(true, false, true)
    val u7 = AvlTreeFlags(true, true, false)
    val u8 = AvlTreeFlags(true, true, true)

    AvlTreeFlags(AvlTreeFlags.serializeFlags(u1)) shouldBe u1
    AvlTreeFlags(AvlTreeFlags.serializeFlags(u2)) shouldBe u2
    AvlTreeFlags(AvlTreeFlags.serializeFlags(u3)) shouldBe u3
    AvlTreeFlags(AvlTreeFlags.serializeFlags(u4)) shouldBe u4
    AvlTreeFlags(AvlTreeFlags.serializeFlags(u5)) shouldBe u5
    AvlTreeFlags(AvlTreeFlags.serializeFlags(u6)) shouldBe u6
    AvlTreeFlags(AvlTreeFlags.serializeFlags(u7)) shouldBe u7
    AvlTreeFlags(AvlTreeFlags.serializeFlags(u8)) shouldBe u8
  }

}
