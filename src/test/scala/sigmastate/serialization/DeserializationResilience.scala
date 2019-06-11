package sigmastate.serialization

import java.nio.ByteBuffer

import org.ergoplatform.{ErgoBoxCandidate, Outputs}
import org.ergoplatform.validation.ValidationException
import org.scalacheck.Gen
import scorex.util.serialization.{Reader, VLQByteBufferReader}
import sigmastate.Values.{IntConstant, SValue, SigmaBoolean, Tuple}
import sigmastate._
import sigmastate.interpreter.CryptoConstants
import sigmastate.lang.exceptions.{DeserializeCallDepthExceeded, InputSizeLimitExceeded, InvalidTypePrefix}
import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer
import sigmastate.serialization.OpCodes._
import sigmastate.utils.SigmaByteReader
import sigmastate.utxo.SizeOf

import scala.collection.mutable

class DeserializationResilience extends SerializationSpecification {

  private def reader(bytes: Array[Byte], maxTreeDepth: Int): SigmaByteReader = {
    val buf = ByteBuffer.wrap(bytes)
    val r = new SigmaByteReader(
      new VLQByteBufferReader(buf),
      new ConstantStore(),
      resolvePlaceholdersToConstants = false,
      maxTreeDepth = maxTreeDepth).mark()
    r
  }

  property("empty") {
    an[ArrayIndexOutOfBoundsException] should be thrownBy ValueSerializer.deserialize(Array[Byte]())
  }

  property("exceeding ergo box propositionBytes max size check") {
    val oversizedTree = new SigmaAnd(
      Gen.listOfN(SigmaSerializer.MaxPropositionSize / CryptoConstants.groupSize,
        proveDlogGen.map(_.toSigmaProp)).sample.get).treeWithSegregation
    val b = new ErgoBoxCandidate(1L, oversizedTree, 1)
    val w = SigmaSerializer.startWriter()
    ErgoBoxCandidate.serializer.serialize(b, w)
    an[InputSizeLimitExceeded] should be thrownBy
      ErgoBoxCandidate.serializer.parse(SigmaSerializer.startReader(w.toBytes))
  }

  property("ergo box propositionBytes max size check") {
    val bigTree = new SigmaAnd(
      Gen.listOfN((SigmaSerializer.MaxPropositionSize / 2) / CryptoConstants.groupSize,
        proveDlogGen.map(_.toSigmaProp)).sample.get).treeWithSegregation
    val b = new ErgoBoxCandidate(1L, bigTree, 1)
    val w = SigmaSerializer.startWriter()
    ErgoBoxCandidate.serializer.serialize(b, w)
    ErgoBoxCandidate.serializer.parse(SigmaSerializer.startReader(w.toBytes))
  }

  property("zeroes (invalid type code in constant deserialization path") {
    an[InvalidTypePrefix] should be thrownBy ValueSerializer.deserialize(Array.fill[Byte](1)(0))
    an[InvalidTypePrefix] should be thrownBy ValueSerializer.deserialize(Array.fill[Byte](2)(0))
  }

  property("default value for max recursive call depth is checked") {
    val evilBytes = List.tabulate(SigmaSerializer.MaxTreeDepth + 1)(_ => Array[Byte](AndCode, ConcreteCollectionCode, 2, SBoolean.typeCode))
      .toArray.flatten
    an[DeserializeCallDepthExceeded] should be thrownBy
      SigmaSerializer.startReader(evilBytes, 0).getValue()
    // test other API endpoints
    an[DeserializeCallDepthExceeded] should be thrownBy
      ValueSerializer.deserialize(evilBytes, 0)
    an[DeserializeCallDepthExceeded] should be thrownBy
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
    an[ValidationException] should be thrownBy
      ValueSerializer.deserialize(Array.fill[Byte](1)(117.toByte))
  }

  private def traceReaderCallDepth(expr: SValue): (IndexedSeq[Int], IndexedSeq[Int]) = {
    class LoggingSigmaByteReader(r: Reader) extends
      SigmaByteReader(r,
        new ConstantStore(),
        resolvePlaceholdersToConstants = false,
        maxTreeDepth = SigmaSerializer.MaxTreeDepth) {
      val levels: mutable.ArrayBuilder[Int] = mutable.ArrayBuilder.make[Int]()
      override def level_=(v: Int): Unit = {
        if (v >= super.level) {
          // going deeper (depth is increasing), save new depth to account added depth level by the caller
          levels += v
        } else {
          // going up (depth is decreasing), save previous depth to account added depth level for the caller
          levels += super.level
        }
        super.level_=(v)
      }
    }

    class ProbeException extends Exception

    class ThrowingSigmaByteReader(r: Reader, levels: IndexedSeq[Int], throwOnNthLevelCall: Int) extends
      SigmaByteReader(r,
        new ConstantStore(),
        resolvePlaceholdersToConstants = false,
        maxTreeDepth = SigmaSerializer.MaxTreeDepth) {
      private var levelCall: Int = 0
      override def level_=(v: Int): Unit = {
        if (throwOnNthLevelCall == levelCall) throw new ProbeException()
        levelCall += 1
        super.level_=(v)
      }
    }

    val bytes = ValueSerializer.serialize(expr)
    val loggingR = new LoggingSigmaByteReader(new VLQByteBufferReader(ByteBuffer.wrap(bytes))).mark()
    val _ = ValueSerializer.deserialize(loggingR)
    val levels = loggingR.levels.result()
    levels.nonEmpty shouldBe true

    val callDepthsBuilder = mutable.ArrayBuilder.make[Int]()
    levels.zipWithIndex.foreach { case (_, levelIndex) =>
      val throwingR = new ThrowingSigmaByteReader(new VLQByteBufferReader(ByteBuffer.wrap(bytes)),
        levels,
        throwOnNthLevelCall = levelIndex).mark()
      try {
        val _ = ValueSerializer.deserialize(throwingR)
      } catch {
        case e: Exception =>
          e.isInstanceOf[ProbeException] shouldBe true
          val stackTrace = e.getStackTrace
          val depth = stackTrace.count { se =>
            (se.getClassName == ValueSerializer.getClass.getName && se.getMethodName == "deserialize") ||
              (se.getClassName == DataSerializer.getClass.getName && se.getMethodName == "deserialize") ||
              (se.getClassName == SigmaBoolean.serializer.getClass.getName && se.getMethodName == "parse")
          }
          callDepthsBuilder += depth
      }
    }
    (levels, callDepthsBuilder.result())
  }

  property("reader.level correspondence to the serializer recursive call depth") {
    forAll(logicalExprTreeNodeGen(Seq(AND.apply, OR.apply))) { expr =>
      val (callDepths, levels) = traceReaderCallDepth(expr)
      callDepths shouldEqual levels
    }
    forAll(numExprTreeNodeGen) { numExpr =>
      val expr = EQ(numExpr, IntConstant(1))
      val (callDepths, levels) = traceReaderCallDepth(expr)
      callDepths shouldEqual levels
    }
    forAll(sigmaBooleanGen) { sigmaBool =>
      val (callDepths, levels) = traceReaderCallDepth(sigmaBool)
      callDepths shouldEqual levels
    }
  }

  property("reader.level is updated in ValueSerializer.deserialize") {
    val expr = SizeOf(Outputs)
    val (callDepths, levels) = traceReaderCallDepth(expr)
    callDepths shouldEqual levels
    callDepths shouldEqual IndexedSeq(1, 2, 2, 1)
  }

  property("max recursive call depth is checked in reader.level for ValueSerializer calls") {
    val expr = SizeOf(Outputs)
    an[DeserializeCallDepthExceeded] should be thrownBy
      ValueSerializer.deserialize(reader(ValueSerializer.serialize(expr), maxTreeDepth = 1))
  }

  property("reader.level is updated in DataSerializer.deserialize") {
    val expr = IntConstant(1)
    val (callDepths, levels) = traceReaderCallDepth(expr)
    callDepths shouldEqual levels
    callDepths shouldEqual IndexedSeq(1, 2, 2, 1)
  }

  property("max recursive call depth is checked in reader.level for DataSerializer calls") {
    val expr = IntConstant(1)
    an[DeserializeCallDepthExceeded] should be thrownBy
      ValueSerializer.deserialize(reader(ValueSerializer.serialize(expr), maxTreeDepth = 1))
  }

  property("reader.level is updated in SigmaBoolean.serializer.parse") {
    val expr = CAND(Seq(proveDlogGen.sample.get, proveDHTGen.sample.get))
    val (callDepths, levels) = traceReaderCallDepth(expr)
    callDepths shouldEqual levels
    callDepths shouldEqual IndexedSeq(1, 2, 3, 4, 4, 4, 4, 3, 2, 1)
  }

  property("max recursive call depth is checked in reader.level for SigmaBoolean.serializer calls") {
    val expr = CAND(Seq(proveDlogGen.sample.get, proveDHTGen.sample.get))
    an[DeserializeCallDepthExceeded] should be thrownBy
      ValueSerializer.deserialize(reader(ValueSerializer.serialize(expr), maxTreeDepth = 1))
  }

  property("reader.level is updated in TypeSerializer") {
    val expr = Tuple(Tuple(IntConstant(1), IntConstant(1)), IntConstant(1))
    val (callDepths, levels) = traceReaderCallDepth(expr)
    callDepths shouldEqual levels
    callDepths shouldEqual IndexedSeq(1, 2, 3, 4, 4, 3, 3, 4, 4, 3, 2, 2, 3, 3, 2, 1)
  }

  property("max recursive call depth is checked in reader.level for TypeSerializer") {
    val expr = Tuple(Tuple(IntConstant(1), IntConstant(1)), IntConstant(1))
    an[DeserializeCallDepthExceeded] should be thrownBy
      ValueSerializer.deserialize(reader(ValueSerializer.serialize(expr), maxTreeDepth = 3))
  }

}
