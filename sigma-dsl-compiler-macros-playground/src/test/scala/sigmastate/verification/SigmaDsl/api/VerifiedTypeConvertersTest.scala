package sigmastate.verification.SigmaDsl.api

import java.util

import org.scalacheck.Arbitrary.arbLong
import sigmastate.SCollection._
import sigmastate.Values.{ByteArrayConstant, ConcreteCollection, LongArrayConstant}
import sigmastate.eval.CSigmaProp
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.serialization.generators.ObjectGenerators
import sigmastate.verification.SigmaDsl.api.collection.{Coll => VerifiedColl}
import sigmastate.verification.SigmaDsl.api.sigma.{SigmaProp => VSigmaProp}
import special.collection.Coll
import special.sigma.SigmaProp

class VerifiedTypeConvertersTest extends SigmaTestingCommons with ObjectGenerators {

  import sigmastate.verification.SigmaDsl.api.VerifiedTypeConverters._

  property("Coll[Coll[Byte]]") {
    forAll(byteCollGen(0, 10)) { ba =>
      val vca: VerifiedColl[Byte] = VerifiedColl(ba.toArray)
      val vcaa: Array[VerifiedColl[Byte]] = ba.toArray.map(_ => vca)
      val v: VerifiedColl[VerifiedColl[Byte]] = VerifiedColl(vcaa)
      val c: Coll[Coll[Byte]] = v
      assert(util.Arrays.deepEquals(c.toArray.map(_.toArray), v.toArray.map(_.toArray)))
      val tree = VCollToErgoTree.to(v)
      val expectedTree = ConcreteCollection(vcaa.map(ByteArrayConstant(_)) ,SByteArray)
      assert(tree == expectedTree)
    }
  }

  property("Coll[Coll[Long]]") {
    forAll(byteCollGen(0, 10)) { ba =>
      val vca: VerifiedColl[Long] = VerifiedColl(ba.map(_.toLong * 1000).toArray)
      val vcaa: Array[VerifiedColl[Long]] = ba.toArray.map(_ => vca)
      val v: VerifiedColl[VerifiedColl[Long]] = VerifiedColl(vcaa)
      val c: Coll[Coll[Long]] = v
      assert(util.Arrays.deepEquals(c.toArray.map(_.toArray), v.toArray.map(_.toArray)))
      val tree = VCollToErgoTree.to(v)
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
      val c: Coll[Coll[Coll[Byte]]] = v
      assert(
        util.Arrays.deepEquals(c.toArray.map(_.toArray.map(_.toArray)),
        v.toArray.map(_.toArray.map(_.toArray)))
      )
    }
  }

  property("Coll[(Coll[Byte], Long)]") {
    forAll(byteCollGen(0, 10)) { ba =>
      val vca: VerifiedColl[Byte] = VerifiedColl(ba.toArray)
      val vcaa: Array[(VerifiedColl[Byte], Long)] = ba.toArray.map(_ => (vca, arbLong.arbitrary.sample.get))
      val v: VerifiedColl[(VerifiedColl[Byte], Long)] = VerifiedColl(vcaa)
      val c: Coll[(Coll[Byte], Long)] = v
      //      assert(util.Arrays.deepEquals(c.toArray.map(_.toArray), v.toArray.map(_.toArray)))
//      val tree = verifiedCollToTree(v)
//      val expectedTree = ConcreteCollection(vcaa.map(ByteArrayConstant(_)), SByteArray)
//      assert(tree == expectedTree)
    }
  }

  property("VSigmaPropToSigmaProp") {
    forAll(proveDlogGen) { pd =>
      val prop: SigmaProp = CSigmaProp(pd)
      Iso.roundTrip(prop) shouldEqual prop
    }
  }
}
