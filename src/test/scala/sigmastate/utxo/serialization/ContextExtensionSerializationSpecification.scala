package sigmastate.utxo.serialization

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks, TableDrivenPropertyChecks}
import sigmastate.Values.{IntConstant, TrueLeaf}
import sigmastate.interpreter.ContextExtension
import sigmastate.serialization.generators._

class ContextExtensionSerializationSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with TableDrivenPropertyChecks
  with Matchers
  with ValueGenerators
  with ConcreteCollectionGenerators
  with TransformerGenerators
  with RelationGenerators {

  property("two extension roundtrip") {
    val serializer = ContextExtension.serializer

    val ext1 = ContextExtension(Map((21: Byte) -> IntConstant(2000), (22: Byte) -> TrueLeaf))
    val ext2 = ContextExtension(Map((22: Byte) -> IntConstant(10000)))
    val ext3 = ContextExtension(Map())

    val bs = serializer.toBytes(ext1) ++ serializer.toBytes(ext2) ++ serializer.toBytes(ext3)

    val (e1, consumed1) = serializer.parseBody(bs, 0)
    val (e2, consumed2) = serializer.parseBody(bs, consumed1)
    val (e3, consumed3) = serializer.parseBody(bs, consumed1 + consumed2)

    consumed1 + consumed2 + consumed3 shouldBe bs.length

    e1.values(21).value shouldBe 2000
    e1.values(22).value shouldBe true

    e2.values(22).value shouldBe 10000

    e3.values.isEmpty shouldBe true
  }

}
