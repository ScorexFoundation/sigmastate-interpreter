package sigmastate.serialization.generators

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import sigmastate.Values.{IntConstant, TaggedBox, TaggedInt}

trait SingleValueGenerators {

  implicit val arbIntConstants: Arbitrary[IntConstant] = Arbitrary(intConstGen)
  implicit val arbTaggedInt: Arbitrary[TaggedInt] = Arbitrary(taggedIntGen)
  implicit val arbTaggedBox: Arbitrary[TaggedBox] = Arbitrary(taggedBoxGen)

  val intConstGen: Gen[IntConstant] = arbLong.arbitrary.map{v => IntConstant(v)}
  val taggedIntGen: Gen[TaggedInt] = arbByte.arbitrary.map{v => TaggedInt(v)}
  val taggedBoxGen: Gen[TaggedBox] = arbByte.arbitrary.map{v => TaggedBox(v)}


}
