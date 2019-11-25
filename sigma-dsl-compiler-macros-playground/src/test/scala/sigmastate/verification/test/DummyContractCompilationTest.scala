package sigmastate.verification.test

import org.ergoplatform.Height
import sigmastate.{BinAnd, BoolToSigmaProp, GE, GT, LE, LT, SCollection, STuple, SType, SigmaAnd}
import sigmastate.Values.{ByteArrayConstant, IntConstant, LongArrayConstant, LongConstant, SigmaPropConstant, Value}
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.utxo.{ByIndex, SelectField, SizeOf}
import sigmastate.verification.contract.DummyContractCompilation
import sigmastate.verified.VerifiedTypeConverters._
import org.scalacheck.Arbitrary.arbLong
import sigmastate.eval.CSigmaProp
import special.collection.{Coll, CollOverArray}
import special.sigma.SigmaProp

class DummyContractCompilationTest extends SigmaTestingCommons with MiscGenerators {

  implicit lazy val IR: TestingIRContext = new TestingIRContext

  private val greaterThanMaxHeight = 10000000

  property("dummy contract ergo tree") {
    forAll(unsignedIntGen) { l =>
      val c = DummyContractCompilation.contractInstance(l)
      val expectedProp = BoolToSigmaProp(LT(Height, IntConstant(l)))
      assert(c.prop == expectedProp)
    }
  }

  property("dummy contract scalaFunc") {
    val contractTrue = DummyContractCompilation.contractInstance(greaterThanMaxHeight)
    val contractFalse = DummyContractCompilation.contractInstance(0)
    forAll(ergoLikeContextGen.map(_.toSigmaContext(IR, isCost = false, Map()))) { ctx =>
      // "should be" matcher brakes the scalac compiler (probably due to stainless)
      assert(contractTrue.scalaFunc(ctx).isValid)
      assert(!contractFalse.scalaFunc(ctx).isValid)
    }
  }

  property("dummy contract2 ergo tree") {
    forAll(unsignedIntGen) { l =>
      val s = l
      val e = l + 9
      val c = DummyContractCompilation.contract2Instance(s, e)
      val expectedProp = BoolToSigmaProp(BinAnd(GE(Height, IntConstant(s)), LE(Height, LongConstant(e))))
      assert(c.prop == expectedProp)
    }
  }

  property("dummy contract2 scalaFunc") {
    val contractTrue = DummyContractCompilation.contract2Instance(0, greaterThanMaxHeight)
    val contractFalse = DummyContractCompilation.contract2Instance(greaterThanMaxHeight, greaterThanMaxHeight)
    forAll(ergoLikeContextGen.map(_.toSigmaContext(IR, isCost = false, Map()))) { ctx =>
      // "should be" matcher brakes the scalac compiler (probably due to stainless)
      assert(contractTrue.scalaFunc(ctx).isValid)
      assert(!contractFalse.scalaFunc(ctx).isValid)
    }
  }

  property("dummy contract3 ergo tree") {
    forAll(byteCollGen(0, 100)) { ba =>
      val la = ba.map(_.toLong)
      val c = DummyContractCompilation.contract3Instance(ba, la)
      val expectedProp = BoolToSigmaProp(
        BinAnd(
          GT(SizeOf(ByteArrayConstant(ba)), IntConstant(0)),
          GT(SizeOf(LongArrayConstant(la)), IntConstant(0))
        )
      )
      assert(c.prop == expectedProp)
    }
  }

  property("dummy contract3 scalaFunc") {
    val ba = byteCollGen(1, 100).sample.get
    val la = ba.map(_.toLong)
    val emptyByteColl = byteCollGen(0).sample.get
    val emptyLongColl = emptyByteColl.map(_.toLong)
    val contractTrue = DummyContractCompilation.contract3Instance(ba, la)
    val contractFalse = DummyContractCompilation.contract3Instance(emptyByteColl, emptyLongColl)
    forAll(ergoLikeContextGen.map(_.toSigmaContext(IR, isCost = false, Map()))) { ctx =>
      assert(contractTrue.scalaFunc(ctx).isValid)
      assert(!contractFalse.scalaFunc(ctx).isValid)
    }
  }

  property("dummy contract4 ergo tree") {
    forAll(proveDlogGen) { proveDlog =>
      val c = DummyContractCompilation.contract4Instance(CSigmaProp(proveDlog).asInstanceOf[SigmaProp])
      val expectedProp = SigmaAnd(BoolToSigmaProp(GE(Height, IntConstant(0))), SigmaPropConstant(proveDlog))
      assert(c.prop == expectedProp)
    }
  }

  property("dummy contract5 ergo tree") {
    forAll(proveDlogGen, proveDlogGen) { case (proveDlog1, proveDlog2) =>
      val c = DummyContractCompilation.contract5Instance(
        CSigmaProp(proveDlog1).asInstanceOf[SigmaProp],
        CSigmaProp(proveDlog2).asInstanceOf[SigmaProp])
      val expectedProp = SigmaAnd(SigmaPropConstant(proveDlog1), SigmaPropConstant(proveDlog2))
      assert(c.prop == expectedProp)
    }
  }

  property("dummy contract6 ergo tree") {
    forAll(collOfGen(byteCollGen(0, 10), 10)) { cc =>
      val c = DummyContractCompilation.contract6Instance(cc)
      val collTree = VCollToErgoTree.to(cc)
      val expectedProp = BoolToSigmaProp(
        GT(
          SizeOf(ByIndex(collTree, IntConstant(0)).asInstanceOf[Value[SCollection[SType]]]),
          IntConstant(0)
        ),
      )
      assert(c.prop == expectedProp)
    }
  }

  property("dummy contract6 scalaFunc") {
    val cc = collOfGen(byteCollGen(1, 10), 10).sample.get
    val contractTrue = DummyContractCompilation.contract6Instance(cc)
    val firstItemEmpty = cc.append(
      new CollOverArray[Coll[Byte]](Array[Coll[Byte]](new CollOverArray[Byte](Array.empty[Byte])))
    ).reverse
    val contractFalse = DummyContractCompilation.contract6Instance(firstItemEmpty)
    forAll(ergoLikeContextGen.map(_.toSigmaContext(IR, isCost = false, Map()))) { ctx =>
      assert(contractTrue.scalaFunc(ctx).isValid)
      assert(!contractFalse.scalaFunc(ctx).isValid)
    }
  }

  property("dummy contract7 ergo tree") {
    forAll(collOfGen(byteCollGen(0, 10).map((_, arbLong.arbitrary.sample.get)), 10)) { tc =>
      val c = DummyContractCompilation.contract7Instance(tc)
      val collTree = VCollToErgoTree.to(tc)
      val expectedProp = BoolToSigmaProp(
        GT(
          SizeOf(
            SelectField(
              ByIndex(collTree, IntConstant(0)).asInstanceOf[Value[STuple]],
              1
            ).asInstanceOf[Value[SCollection[SType]]]),
          IntConstant(0)
        ),
      )
      assert(c.prop == expectedProp)
    }
  }

}
