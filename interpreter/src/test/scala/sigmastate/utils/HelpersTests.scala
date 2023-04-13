package sigmastate.utils

import sigmastate.serialization.generators.ObjectGenerators
import Helpers._
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class HelpersTests extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers with ObjectGenerators {
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
      printDebug(arr.length)
    }
  }
}
