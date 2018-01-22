package sigmastate.serializer.bytes.base

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import sigmastate._

class TypeBytesSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  property("Types.typeCode should return correct type code") {
    TypeBytes.typeCode[SInt.type] shouldBe Array(1)
    TypeBytes.typeCode[SBigInt.type] shouldBe Array(2)
    TypeBytes.typeCode[SBoolean.type] shouldBe Array(3)
    TypeBytes.typeCode[SByteArray.type] shouldBe Array(4)
    TypeBytes.typeCode[SProp.type] shouldBe Array(5)
    TypeBytes.typeCode[SAvlTree.type] shouldBe Array(6)
    TypeBytes.typeCode[SGroupElement.type] shouldBe Array(7)
    TypeBytes.typeCode[SBox.type] shouldBe Array(8)
    TypeBytes.typeCode[SCollection[SBox.type]] shouldBe Array(9, 8)
    TypeBytes.typeCode[SCollection[SCollection[SInt.type]]] shouldBe Array(9, 9, 1)
  }

  property("Types.checkCollectionType should check that the seq has correct type") {
    TypeBytes.checkCollectionType[SBoolean.type](Array(1), Seq(BooleanConstant.fromBoolean(true))) shouldBe false
    TypeBytes.checkCollectionType[SBoolean.type](Array(3), Seq(BooleanConstant.fromBoolean(true))) shouldBe true
    TypeBytes.checkCollectionType[SInt.type](Array(1), Seq(IntConstant(1))) shouldBe true
    TypeBytes.checkCollectionType[SInt.type](Array(2), Seq(IntConstant(1))) shouldBe false
  }
}
