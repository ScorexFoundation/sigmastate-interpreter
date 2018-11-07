package sigmastate.serialization

import org.scalacheck.Gen
import sigmastate.SType
import sigmastate.Values.Constant
import sigmastate.lang.{DeserializationSigmaBuilder, SigmaBuilder}

class BlockSerializerSpecification extends SerializationSpecification {

  property("ValDef: serializer round trip") {
    forAll(valOrFunDefGen) { v =>
      roundTripTest(v)
    }
  }

  property("BlockValue and ValUse: serializer round trip") {
    forAll(blockValueGen) { v =>
      roundTripTest(v)
    }
  }

  property("ConstantPlaceholder: serializer round trip") {
    forAll(Gen.oneOf(byteConstGen, intConstGen, groupElementConstGen, booleanConstGen)) { v =>
      implicit val builder: SigmaBuilder = DeserializationSigmaBuilder
      val store = new ConstantStore(IndexedSeq())
      val placeholder = store.put(v.asInstanceOf[Constant[SType]])
      val s = ConstantPlaceholderSerializer(DeserializationSigmaBuilder.mkConstantPlaceholder)
      val w = Serializer.startWriter()
      s.serializeBody(placeholder, w)
      val r = Serializer.startReader(w.toBytes, store)
      s.parseBody(r) shouldEqual placeholder
    }
  }

  property("FuncValue: serializer round trip") {
    forAll(funcValueGen) { v =>
      roundTripTest(v)
    }
  }
}
