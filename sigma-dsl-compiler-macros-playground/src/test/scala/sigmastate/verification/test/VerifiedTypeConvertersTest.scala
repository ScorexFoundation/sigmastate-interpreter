package sigmastate.verification.test

import org.scalacheck.Arbitrary.arbLong
import sigmastate.SCollection._
import sigmastate.Values
import sigmastate.Values.{ByteArrayConstant, ConcreteCollection}
import sigmastate.eval.CSigmaProp
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.verified.VerifiedTypeConverters._
import sigmastate.verified.Iso
import special.collection._
import stainless.annotation.ignore

@ignore
class VerifiedTypeConvertersTest extends SigmaTestingCommons with MiscGenerators {

  property("Coll[Coll[Byte]]") {
    forAll(collOfGen(byteCollGen(0, 10), 10)) { cc =>
      assert(Iso.roundTrip(cc) == cc)
      val tree = VCollToErgoTree.to(cc)
      val expectedTree = ConcreteCollection(cc.toArray.map(ByteArrayConstant(_)) ,SByteArray)
      assert(tree == expectedTree)
    }
  }

  property("Coll[Coll[Long]]") {
    forAll(collOfGen(byteCollGen(0, 10).map(bs => bs.map(_.toLong)), 10)) { cc =>
      assert(Iso.roundTrip(cc) == cc)
      val tree = VCollToErgoTree.to(cc)
      val expectedTree = ConcreteCollection(cc.toArray.map(Values.LongArrayConstant(_)), SLongArray)
      assert(tree == expectedTree)
    }
  }

  property("Coll[Coll[Coll[Byte]]]") {
    forAll(collOfGen(collOfGen(byteCollGen(0, 10), 10), 10)) { ccc =>
      assert(Iso.roundTrip(ccc) == ccc)
    }
  }

  property("Coll[(Coll[Byte], Long)]") {
    forAll(collOfGen(byteCollGen(0, 10).map((_, arbLong.arbitrary.sample.get)), 10)) { tc =>
      assert(Iso.roundTrip(tc) == tc)
    }
  }

  property("VSigmaPropToSigmaProp") {
    forAll(proveDlogGen.map(CSigmaProp)) { sp =>
      assert(Iso.roundTrip(sp) == sp)
    }
  }
}
