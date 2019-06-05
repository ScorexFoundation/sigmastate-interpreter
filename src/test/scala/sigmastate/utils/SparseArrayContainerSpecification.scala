package sigmastate.utils

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import sigmastate.serialization.generators.ValueGenerators

class SparseArrayContainerSpecification extends PropSpec
  with ValueGenerators
  with PropertyChecks
  with Matchers {

  private val distinctCodeValuePairsGen: Gen[Seq[(Byte, Long)]] = for {
    bytes <- distinctListOfGen(Arbitrary.arbByte.arbitrary)
    longs <- Gen.listOfN(bytes.length, Arbitrary.arbLong.arbitrary)
  } yield bytes.zip(longs)

  property("distinct codes") {
    val codeValuePairs = Seq[(Byte, Long)]((1, 1), (1, 2))
    an[RuntimeException] should be thrownBy
      new SparseArrayContainer(codeValuePairs)
  }

  property("get") {
    forAll(distinctCodeValuePairsGen) { codeValuePairs =>
      val cont = new SparseArrayContainer(codeValuePairs)
      codeValuePairs.foreach { case (code, v) =>
          cont(code) shouldBe v
      }
      val mappedValues = codeValuePairs.toMap
      (Byte.MinValue to Byte.MaxValue).foreach { i =>
        if (mappedValues.get(i.toByte).isEmpty)
          cont(i.toByte) shouldBe null.asInstanceOf[Long]
      }
    }
  }
}
