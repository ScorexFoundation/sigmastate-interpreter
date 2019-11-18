package sigmastate.verification.contract

import java.util

import sigmastate.Values.{ByteArrayConstant, ConcreteCollection, LongArrayConstant}
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.serialization.generators.ObjectGenerators
import sigmastate.verification.SigmaDsl.api.collection.{Coll => VerifiedColl}
import sigmastate.SCollection._
import special.collection.Coll
import stainless.annotation.ignore

@ignore
class VerifiedTypeConvertersTest extends SigmaTestingCommons with ObjectGenerators {

  import sigmastate.verification.SigmaDsl.api.VerifiedTypeConverters._

  property("Coll[Coll[Byte]]") {
    forAll(byteCollGen(0, 10)) { ba =>
      val vca: VerifiedColl[Byte] = VerifiedColl(ba.toArray)
      val vcaa: Array[VerifiedColl[Byte]] = ba.toArray.map(_ => vca)
      val v: VerifiedColl[VerifiedColl[Byte]] = VerifiedColl(vcaa)
      val c: Coll[Coll[Byte]] = verifiedCollToColl(v)
      assert(util.Arrays.deepEquals(c.toArray.map(_.toArray), v.toArray.map(_.toArray)))
      val tree = verifiedCollToTree(v)
      val expectedTree = ConcreteCollection(vcaa.map(ByteArrayConstant(_)) ,SByteArray)
      assert(tree == expectedTree)
    }
  }

  property("Coll[Coll[Long]]") {
    forAll(byteCollGen(0, 10)) { ba =>
      val vca: VerifiedColl[Long] = VerifiedColl(ba.map(_.toLong * 1000).toArray)
      val vcaa: Array[VerifiedColl[Long]] = ba.toArray.map(_ => vca)
      val v: VerifiedColl[VerifiedColl[Long]] = VerifiedColl(vcaa)
      val c: Coll[Coll[Long]] = verifiedCollToColl(v)
      assert(util.Arrays.deepEquals(c.toArray.map(_.toArray), v.toArray.map(_.toArray)))
      val tree = verifiedCollToTree(v)
      val expectedTree = ConcreteCollection(vcaa.map(LongArrayConstant(_)), SLongArray)
      assert(tree == expectedTree)
    }
  }

  property("Coll[Coll[Coll[Byte]]]") {
    forAll(byteCollGen(0, 10)) { ba =>
      val vca: VerifiedColl[Byte] = VerifiedColl(ba.toArray)
      val vcaa: Array[VerifiedColl[Byte]] = ba.toArray.map(_ => vca)
      val vcaaa: Array[VerifiedColl[VerifiedColl[Byte]]] = Array(VerifiedColl(vcaa))
      val v: VerifiedColl[VerifiedColl[VerifiedColl[Byte]]] = VerifiedColl(vcaaa)
      val c: Coll[Coll[Coll[Byte]]] = verifiedCollToColl(v)
      assert(
        util.Arrays.deepEquals(c.toArray.map(_.toArray.map(_.toArray)),
        v.toArray.map(_.toArray.map(_.toArray)))
      )
    }
  }

}
