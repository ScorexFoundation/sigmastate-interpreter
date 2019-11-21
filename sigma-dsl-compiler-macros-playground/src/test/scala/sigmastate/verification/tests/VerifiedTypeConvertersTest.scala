package sigmastate.verification.tests

import java.util

import org.scalacheck.Arbitrary.arbLong
import org.scalacheck.Gen
import scalan.RType
import sigmastate.SCollection._
import sigmastate.Values
import sigmastate.Values.{ByteArrayConstant, ConcreteCollection, LongArrayConstant}
import sigmastate.eval.CSigmaProp
import sigmastate.eval.Extensions._
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.serialization.generators.ObjectGenerators
import sigmastate.verification.SigmaDsl.api.Iso
import sigmastate.verification.SigmaDsl.api.VerifiedTypeConverters._
import sigmastate.verification.SigmaDsl.api.collection.{Coll => VColl}
import special.collection.{Coll, _}

class VerifiedTypeConvertersTest extends SigmaTestingCommons with ObjectGenerators {

  def collOfGen[A: RType](g: Gen[A], length: Int): Gen[Coll[A]] = Gen.listOfN(length, g).map(_.toColl)

  property("Coll[Coll[Byte]]") {
    forAll(collOfGen(byteCollGen(0, 10), 10)) { cc =>
      Iso.roundTrip(cc) shouldEqual cc
      val tree = VCollToErgoTree.to(cc)
      val expectedTree = ConcreteCollection(cc.toArray.map(ByteArrayConstant(_)) ,SByteArray)
      assert(tree == expectedTree)
    }
  }

  property("Coll[Coll[Long]]") {
    forAll(collOfGen(byteCollGen(0, 10).map(bs => bs.map(_.toLong)), 10)) { cc =>
      Iso.roundTrip(cc) shouldEqual cc
      val tree = VCollToErgoTree.to(cc)
      val expectedTree = ConcreteCollection(cc.toArray.map(Values.LongArrayConstant(_)), SLongArray)
      assert(tree == expectedTree)
    }
  }

  property("Coll[Coll[Coll[Byte]]]") {
    forAll(collOfGen(collOfGen(byteCollGen(0, 10), 10), 10)) { ccc =>
      Iso.roundTrip(ccc) shouldEqual ccc
    }
  }

  property("Coll[(Coll[Byte], Long)]") {
    forAll(collOfGen(byteCollGen(0, 10).map((_, arbLong.arbitrary.sample.get)), 10)) { tc =>
      Iso.roundTrip(tc) shouldEqual tc
    }
  }

  property("VSigmaPropToSigmaProp") {
    forAll(proveDlogGen.map(CSigmaProp)) { sp =>
      Iso.roundTrip(sp) shouldEqual sp
    }
  }
}
