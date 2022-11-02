package sigmastate.utxo

import org.ergoplatform._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{PropSpec, Matchers}
import scalan.util.BenchmarkUtil
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.interpreter.{ProverResult, ContextExtension}
import sigmastate.serialization.generators.ObjectGenerators
import debox.{Buffer => DBuffer}
import sigmastate.lang.exceptions.SerializerException
import sigmastate.util.{MaxArrayLength, safeNewArray}
import spire.algebra._
import spire.std.int._

class SerializationRoundTripSpec extends PropSpec
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ObjectGenerators
  with SigmaTestingCommons {

  case class Run(size: Int, time: Long)

  implicit val orderRun = Order.by((r: Run) => r.size)

  property("ValueSerializer.newArray") {
    safeNewArray[Int](0).length shouldBe 0
    safeNewArray[Int](MaxArrayLength).length shouldBe MaxArrayLength

    // test vector to catch constant changes
    MaxArrayLength shouldBe 100000

    assertExceptionThrown(
      safeNewArray[Int](MaxArrayLength + 1),
      exceptionLike[RuntimeException]("Cannot allocate array of Int"))
  }

  property("ErgoBoxCandidate: Serializer round trip benchmark") {
    val runs = DBuffer.empty[Run]
    forAll(MinSuccessful(20)) { t: ErgoBoxCandidate =>
      val (_, time) = BenchmarkUtil.measureTime {
        var i = 0
        while (i < 100) {
          roundTripTest(t)(ErgoBoxCandidate.serializer)
          i += 1
        }
      }
      runs += Run(t.bytesWithNoRef.length, time)
    }
    runs.sort
    for (r <- runs) {
      println(s"Size: ${r.size}, Time: ${r.time}")
    }
  }

  property("ErgoBoxCandidate: Serializer round trip") {
    forAll { t: ErgoBoxCandidate => roundTripTest(t)(ErgoBoxCandidate.serializer) }
    forAll { t: ErgoBoxCandidate => roundTripTestWithPos(t)(ErgoBoxCandidate.serializer) }
  }

  property("ErgoBox: Serializer round trip") {
    forAll { t: ErgoBox => roundTripTest(t)(ErgoBox.sigmaSerializer) }
    forAll { t: ErgoBox => roundTripTestWithPos(t)(ErgoBox.sigmaSerializer) }
  }

  property("ContextExtension: Serializer round trip") {
    forAll { t: ContextExtension => roundTripTest(t)(ContextExtension.serializer) }
    forAll { t: ContextExtension => roundTripTestWithPos(t)(ContextExtension.serializer) }
  }

  property("SerializedProverResult: Serializer round trip") {
    forAll { t: ProverResult => roundTripTest(t)(ProverResult.serializer) }
    forAll { t: ProverResult => roundTripTestWithPos(t)(ProverResult.serializer) }
  }

  property("Input: Serializer round trip") {
    forAll { t: Input => roundTripTest(t)(Input.serializer) }
    forAll { t: Input => roundTripTestWithPos(t)(Input.serializer) }
  }
}
