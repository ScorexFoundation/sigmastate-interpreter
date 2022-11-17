package org.ergoplatform.sdk

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import special.collections.CollGens

class ExtensionsSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers with CollGens {

  property("Coll.partition") {
    forAll(collGen) { col =>
      val (lsC, rsC) = col.partition(lt0)
      val (ls, rs) = col.toArray.partition(lt0)
      lsC.toArray shouldBe ls
      rsC.toArray shouldBe rs
    }
  }

}
