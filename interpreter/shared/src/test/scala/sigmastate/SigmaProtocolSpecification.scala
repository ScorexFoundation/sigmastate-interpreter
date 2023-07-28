package sigmastate

import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigmastate.TrivialProp.{FalseProp, TrueProp}
import sigmastate.Values.SigmaBoolean
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.ProveDHTuple
import sigmastate.basics.VerifierMessage.Challenge
import sigmastate.crypto.{GF2_192, GF2_192_Poly}
import sigmastate.utils.Helpers
import special.sigma.SigmaTestingData

class SigmaProtocolSpecification extends SigmaTestingData with ScalaCheckPropertyChecks {

  property("CThresholdUncheckedNode equality") {
    val c1 = Challenge @@ Array[Byte](1)
    val c2 = Challenge @@ Array[Byte](2)
    val n0 = CThresholdUncheckedNode(c1, Seq(), 0, None)
    val n1 = CThresholdUncheckedNode(c1, Seq(), 0, None)
    val n2 = CThresholdUncheckedNode(c2, Seq(), 0, None)
    val n3 = CThresholdUncheckedNode(c2, Seq(n1), 0, None)
    val n4 = CThresholdUncheckedNode(c2, Seq(n1), 1, None)
    val p = GF2_192_Poly.interpolate(new Array[Byte](0), new Array[GF2_192](0), new GF2_192(0))
    val n5 = CThresholdUncheckedNode(c2, Seq(n1), 1, Some(p))

    assertResult(true)(n0 == n1)
    assertResult(true)(n1 != n2)
    assertResult(true)(n2 != n3)
    assertResult(true)(n3 != n4)
    assertResult(true)(n4 != n5)
  }

  property("collecting SigmaBoolean leaves") {

    val dlog1 = ProveDlog(Helpers.decodeECPoint("0297c44a12f4eb99a85d298fa3ba829b5b42b9f63798c980ece801cc663cc5fc9e"))
    val dlog2 = ProveDlog(Helpers.decodeECPoint("02af645874c3b53465a5e9d820eb207d6001258c3b708f0d31d7c2e342833dce64"))
    val dht1 = ProveDHTuple(
      Helpers.decodeECPoint("021b4c0f54304b9c427d5c87846dd56a2fa361cd34a6cb8a2090aef043c9383198"),
      Helpers.decodeECPoint("026826a4a9d0ec937c24d72da381ee6b5e74e49fb79a6a23a03fe0aa2cab3448ba"),
      Helpers.decodeECPoint("02535153378ce30df1b31137680748de728f8512d29dfeeb1f331ac6a787cd00d8"),
      Helpers.decodeECPoint("03d00d0174cdffd7ce3b77ef45ef9573c18fb76929fb3340f7ceea8d0be9bf5c4a")
    )
    val and = CAND(Seq(dlog1, dlog2))
    val or = COR(Seq(dlog1, dlog2))
    val th = CTHRESHOLD(1, Seq(dlog1, dht1))
    val th2 = CTHRESHOLD(2, Seq(TrueProp, and, or, th, dlog1, dht1))

    val table = Table(("proposition", "leafs"),
      (TrueProp, Seq()),
      (FalseProp, Seq()),
      (dlog1, Seq(dlog1)),
      (dht1, Seq(dht1)),
      (and, Seq(dlog1, dlog2)),
      (or, Seq(dlog1, dlog2)),
      (th, Seq(dlog1, dht1)),
      (th2, Seq(dlog1, dlog2, dlog1, dlog2, dlog1, dht1, dlog1, dht1))
    )
    forAll(table) { (prop: SigmaBoolean, leafs: Seq[SigmaLeaf]) =>
      prop.leaves shouldBe leafs
    }
    th2.leaves.iterator.distinct.toSet shouldBe Set(dlog1, dlog2, dht1)
  }
}
