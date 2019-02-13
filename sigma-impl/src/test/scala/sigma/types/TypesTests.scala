package sigma.types

import org.scalatest.prop.PropertyChecks
import org.scalatest.{PropSpec, Matchers}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import special.collections.CollGens
import sigma.util.Extensions._

class TypesTests extends PropSpec with PropertyChecks with Matchers with CollGens { testSuite =>

  val genSBoolean: Gen[Boolean] = arbBool.arbitrary.map(CBoolean(_))
  implicit val arbSBoolean = Arbitrary(genSBoolean)
  
  val genSByte: Gen[Byte] = arbByte.arbitrary.map(CByte(_))
  implicit val arbSByte = Arbitrary(genSByte)
  
  val genSInt: Gen[Int] = arbInt.arbitrary.map(CInt(_))
  implicit val arbSInt = Arbitrary(genSInt)
  
  property("Boolean methods") {
    forAll { x: Boolean =>
      x.toByte shouldBe CByte(x.value.toByte)
    }
  }

  property("Byte methods") {
    forAll { x: Byte =>
      x.toInt.value shouldBe x.value.toInt
    }
  }

  property("Int methods") {
    forAll(valGen) { in: scala.Int =>
      val x: Int = CInt(in)
      x.toByte.value shouldBe x.value.toByte
    }
  }
}
