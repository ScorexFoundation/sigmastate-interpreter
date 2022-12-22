package org.ergoplatform.sdk.js

import org.ergoplatform.ErgoBox.{BoxId, TokenId}
import org.ergoplatform.sdk.Extensions.PairCollOps
import org.ergoplatform.sdk.{ExtendedInputBox, Iso}
import org.ergoplatform._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigmastate.SType
import sigmastate.Values.Constant
import sigmastate.eval.Colls
import sigmastate.interpreter.ContextExtension
import sigmastate.serialization.generators.ObjectGenerators
import special.collection.Coll
import special.sigma.GroupElement

import scala.scalajs.js

class IsosSpec  extends AnyPropSpec with Matchers with ObjectGenerators with ScalaCheckPropertyChecks{

  lazy val extendedInputBoxGen: Gen[ExtendedInputBox] = for {
    box <- ergoBoxGen
    extension <- contextExtensionGen
  } yield ExtendedInputBox(box, extension)

  def roundtrip[A,B](iso: Iso[A,B])(b: B): Unit = {
    val invIso = iso.inverse
    invIso.from(invIso.to(b)) shouldBe b
  }

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 30)

  property("Iso.isoStringToArray") {
    forAll() { (bytes: Array[Byte]) =>
      roundtrip(Isos.isoStringToArray)(bytes)
    }
  }

  property("Iso.isoStringToColl") {
    forAll() { (bytes: Coll[Byte]) =>
      roundtrip(Isos.isoStringToColl)(bytes)
    }
  }

  property("Iso.isoStringToGroupElement") {
    forAll() { (bytes: GroupElement) =>
      roundtrip(Isos.isoStringToGroupElement)(bytes)
    }
  }

  property("Iso.isoBoxId") {
    forAll(boxIdGen) { (id: BoxId) =>
      roundtrip(Isos.isoBoxId)(id)
    }
  }

  property("Iso.isoHexStringToConstant") {
    forAll(constantGen, MinSuccessful(100)) { (c: Constant[SType]) =>
      roundtrip(Isos.isoHexStringToConstant)(c)
    }
  }

  property("Iso.isoContextExtension") {
    forAll { (c: ContextExtension) =>
      roundtrip(Isos.isoContextExtension)(c)
    }
  }

  property("Iso.isoUnsignedInput") {
    forAll { (c: UnsignedInput) =>
      roundtrip(Isos.isoUnsignedInput)(c)
    }
  }

  property("Iso.isoDataInput") {
    forAll { (c: DataInput) =>
      roundtrip(Isos.isoDataInput)(c)
    }
  }

  property("Iso.isoAmount") {
    forAll { (c: Long) =>
      roundtrip(Isos.isoAmount)(c)
      Isos.isoAmount.to(js.BigInt(c.toString)) shouldBe c
    }
  }

  property("Iso.isoToken") {
    forAll(tokenIdGen, Arbitrary.arbLong.arbitrary) { (id: TokenId, amount: Long) =>
      roundtrip(Isos.isoToken)((Colls.fromArray(id), amount))
    }
  }

  property("Iso.isoTokenArray") {
    forAll(ergoBoxTokens(tokensGen.sample.get)) { tokens =>
      roundtrip(Isos.isoTokenArray)(tokens.mapFirst(id => Colls.fromArray(id)))
    }
  }

  property("Iso.isoBoxCandidate") {
    forAll { (box: ErgoBoxCandidate) =>
      roundtrip(Isos.isoBoxCandidate)(box)
    }
  }

  property("Iso.isoUnsignedTransaction") {
    forAll { (tx: UnsignedErgoLikeTransaction) =>
      roundtrip(Isos.isoUnsignedTransaction)(tx)
    }
  }

  property("Iso.isoBox") {
    forAll { (b: ErgoBox) =>
      roundtrip(Isos.isoBox)(b)
    }
  }

  property("Iso.isoEIP12UnsignedInput") {
    forAll(extendedInputBoxGen) { (b: ExtendedInputBox) =>
      roundtrip(Isos.isoEIP12UnsignedInput)(b)
    }
  }

}
