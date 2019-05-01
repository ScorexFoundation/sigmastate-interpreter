package sigmastate.serialization

import java.nio.ByteBuffer

import sigmastate.lang.exceptions.{InputSizeLimitExceeded, InvalidOpCode, InvalidTypePrefix, ValueDeserializeCallDepthExceeded}
import sigmastate.serialization.OpCodes._
import scorex.util.serialization.{Reader, VLQByteBufferReader}
import sigmastate.utils.SigmaByteReader
import sigmastate.{AND, OR, SBoolean}

import scala.collection.mutable

class DeserializationResilience extends SerializationSpecification {

  property("empty") {
    an[ArrayIndexOutOfBoundsException] should be thrownBy ValueSerializer.deserialize(Array[Byte]())
  }

  property("max size limit") {
    val bytes = Array.fill[Byte](SigmaSerializer.MaxInputSize + 1)(1)
    an[InputSizeLimitExceeded] should be thrownBy ValueSerializer.deserialize(bytes)
    an[InputSizeLimitExceeded] should be thrownBy ValueSerializer.deserialize(SigmaSerializer.startReader(bytes, 0))
  }

  property("zeroes (invalid type code in constant deserialization path") {
    an[InvalidTypePrefix] should be thrownBy ValueSerializer.deserialize(Array.fill[Byte](1)(0))
    an[InvalidTypePrefix] should be thrownBy ValueSerializer.deserialize(Array.fill[Byte](2)(0))
  }

  property("AND/OR nested crazy deep") {
    val evilBytes = List.tabulate(SigmaSerializer.MaxTreeDepth + 1)(_ => Array[Byte](AndCode, ConcreteCollectionCode, 2, SBoolean.typeCode))
      .toArray.flatten
    an[ValueDeserializeCallDepthExceeded] should be thrownBy
      SigmaSerializer.startReader(evilBytes, 0).getValue()
    // test other API endpoints
    an[ValueDeserializeCallDepthExceeded] should be thrownBy
      ValueSerializer.deserialize(evilBytes, 0)
    an[ValueDeserializeCallDepthExceeded] should be thrownBy
      ValueSerializer.deserialize(SigmaSerializer.startReader(evilBytes, 0))

    // guard should not be tripped up by a huge collection
    val goodBytes = SigmaSerializer.startWriter()
      .putValue(AND(List.tabulate(SigmaSerializer.MaxTreeDepth + 1)(_ => booleanExprGen.sample.get)))
      .toBytes
    ValueSerializer.deserialize(goodBytes, 0)
    // test other API endpoints
    ValueSerializer.deserialize(SigmaSerializer.startReader(goodBytes, 0))
    SigmaSerializer.startReader(goodBytes, 0).getValue()
  }

  property("invalid op code") {
    an[InvalidOpCode] should be thrownBy
      ValueSerializer.deserialize(Array.fill[Byte](1)(117.toByte))
  }

  property("reader.level correspondence to the serializer recursive call depth") {

    class LoggingSigmaByteReader(r: Reader) extends
      SigmaByteReader(r, new ConstantStore(), resolvePlaceholdersToConstants = false) {
      val levels: mutable.ArrayBuilder[Int] = mutable.ArrayBuilder.make[Int]()
      override def level_=(v: Int): Unit = {
        levels += v
        super.level_=(v)
      }
    }

    class ProbeException extends Exception

    class ThrowingSigmaByteReader(r: Reader, levels: IndexedSeq[Int], throwOnNthLevelCall: Int) extends
      SigmaByteReader(r, new ConstantStore(), resolvePlaceholdersToConstants = false) {
      private var levelCall: Int = 0
      override def level_=(v: Int): Unit = {
        if (throwOnNthLevelCall == levelCall) throw new ProbeException()
        levelCall += 1
        super.level_=(v)
      }
    }

    forAll(logicalExprTreeNodeGen(Seq(AND.apply, OR.apply))) { tree =>
      val bytes = ValueSerializer.serialize(tree)
      val r = new LoggingSigmaByteReader(new VLQByteBufferReader(ByteBuffer.wrap(bytes))).mark()
      val deserializedTree = ValueSerializer.deserialize(r)
      deserializedTree shouldEqual tree
      val levels = r.levels.result()
      levels.nonEmpty shouldBe true

      levels.zipWithIndex.foreach { case (level, levelIndex) =>
        val throwingR = new ThrowingSigmaByteReader(new VLQByteBufferReader(ByteBuffer.wrap(bytes)),
          levels,
          throwOnNthLevelCall = levelIndex).mark()
        try {
          val _ = ValueSerializer.deserialize(throwingR)
        } catch {
          case e: Exception =>
            e.isInstanceOf[ProbeException] shouldBe true
            val stackTrace = e.getStackTrace
            val depth = stackTrace.count{ se =>
              se.getClassName.contains("ValueSerializer") && se.getMethodName == "deserialize"
            }
            depth shouldBe level
        }
      }
    }
  }
}
