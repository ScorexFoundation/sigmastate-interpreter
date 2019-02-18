package sigmastate.utils

import org.scalatest.prop.PropertyChecks
import org.scalatest.{PropSpec, Matchers}
import sigmastate.serialization.generators.ValueGenerators
import Helpers._

class HelpersTests extends PropSpec with PropertyChecks with Matchers with ValueGenerators {
  property("xorU") {
    forAll(arrayGen[Byte]) { arr =>

      val x = xor(arr, arr)
      val cloned = arr.clone()
      xorU(cloned, arr)
      cloned shouldBe x

      val arr1 = x
      val arr2 = cloned
      val arr3 = xor(arr1, arr2)
      val res1 = xor(cloned, arr1, arr2, arr3)
      val res2 = cloned
      xorU(res2, Seq(arr1, arr2, arr3))

      res1 shouldBe res2
      println(arr.length)
    }
  }
}
