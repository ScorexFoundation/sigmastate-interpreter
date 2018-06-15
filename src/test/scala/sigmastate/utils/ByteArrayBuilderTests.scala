package sigmastate.utils

import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class ByteArrayBuilderTests extends PropSpec with PropertyChecks with Matchers {

  property("Append basic types") {
    val b = new ByteArrayBuilder(1)

    b.append(1.toByte)  // Byte
    b.toBytes shouldBe(Array[Byte](1))
    b.capacity() shouldBe 1

    b.append(1) // Int
    b.toBytes shouldBe(Array[Byte](1, 0, 0, 0, 1))
    b.capacity() shouldBe 5

    b.append(1L << 32) // Long
    b.toBytes shouldBe(Array[Byte](1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0))
    b.capacity() shouldBe 13

    b.append(Array[Byte](10, 20)) // Long
    b.toBytes shouldBe(Array[Byte](1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 10, 20))
    b.capacity() shouldBe 26

  }

}
