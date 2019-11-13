package sigmastate.verification.contract

import org.ergoplatform.Height
import sigmastate.{BinAnd, BoolToSigmaProp, GE, GT, LE, LT, SigmaAnd}
import sigmastate.Values.{ByteArrayConstant, IntConstant, LongArrayConstant, LongConstant, SigmaPropConstant}
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.serialization.generators.ObjectGenerators
import sigmastate.utxo.SizeOf
import sigmastate.verification.SigmaDsl.api.collection.{Coll => VerifiedColl}
import sigmastate.verification.SigmaDsl.api.sigma.{ProveDlogProof, SigmaPropProof}
import special.collection.{Coll, CollOverArray}
import stainless.annotation.ignore

@ignore
class DummyContractCompilationTest extends SigmaTestingCommons with ObjectGenerators {

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
      val c = DummyContractCompilation.contract3Instance(VerifiedColl(ba.toArray), VerifiedColl(la.toArray))
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
    val contractTrue = DummyContractCompilation.contract3Instance(VerifiedColl(ba.toArray), VerifiedColl(la.toArray))
    val contractFalse = DummyContractCompilation.contract3Instance(VerifiedColl.empty[Byte], VerifiedColl.empty[Long])
    forAll(ergoLikeContextGen.map(_.toSigmaContext(IR, isCost = false, Map()))) { ctx =>
      assert(contractTrue.scalaFunc(ctx).isValid)
      assert(!contractFalse.scalaFunc(ctx).isValid)
    }
  }

  property("dummy contract4 ergo tree") {
    forAll(proveDlogGen) { proveDlog =>
      val verifiedProveDlog = SigmaPropProof(ProveDlogProof(proveDlog.value))
      val c = DummyContractCompilation.contract4Instance(verifiedProveDlog)
      val expectedProp = SigmaAnd(BoolToSigmaProp(GE(Height, IntConstant(0))), SigmaPropConstant(proveDlog))
      assert(c.prop == expectedProp)
    }
  }

  property("dummy contract5 ergo tree") {
    forAll(proveDlogGen, proveDlogGen) { case (proveDlog1, proveDlog2) =>
      val verifiedProveDlog1 = SigmaPropProof(ProveDlogProof(proveDlog1.value))
      val verifiedProveDlog2 = SigmaPropProof(ProveDlogProof(proveDlog2.value))
      val c = DummyContractCompilation.contract5Instance(verifiedProveDlog1, verifiedProveDlog2)
      val expectedProp = SigmaAnd(SigmaPropConstant(proveDlog1) , SigmaPropConstant(proveDlog2))
      assert(c.prop == expectedProp)
    }
  }

}
