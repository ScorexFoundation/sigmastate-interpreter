package org.ergoplatform.sdk.js

import org.ergoplatform.ErgoBox.{AdditionalRegisters, BoxId, TokenId}
import org.ergoplatform._
import org.ergoplatform.sdk.wallet.protocol.context.{BlockchainStateContext, CBlockchainStateContext}
import org.ergoplatform.sdk.ExtendedInputBox
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.ast.SType
import sigma.data.Iso
import sigma.js.AvlTree
import sigma.{Coll, Colls, GroupElement}
import sigma.ast.Constant
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.serialization.generators.ObjectGenerators

import scala.scalajs.js

class IsosSpec  extends AnyPropSpec with Matchers with ObjectGenerators with ScalaCheckPropertyChecks{

  lazy val extendedInputBoxGen: Gen[ExtendedInputBox] = for {
    box <- ergoBoxGen
    extension <- contextExtensionGen
  } yield ExtendedInputBox(box, extension)

  lazy val blockchainStateContextGen: Gen[BlockchainStateContext] = for {
    stateRoot <- avlTreeGen
    headers <- headersGen(stateRoot)
    preHeader <- preHeaderGen(headers.headOption.map(_.id).getOrElse(modifierIdBytesGen.sample.get))
  } yield CBlockchainStateContext(
      sigmaLastHeaders = Colls.fromItems(headers:_*),
      previousStateDigest = stateRoot.digest,
      sigmaPreHeader = preHeader
    )

  def roundtrip[A,B](iso: Iso[A,B])(b: B): Unit = {
    iso.to(iso.from(b)) shouldBe b
  }

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 30)

  property("Iso.isoStringToArray") {
    forAll() { (bytes: Array[Byte]) =>
      roundtrip(Iso.isoStringToArray)(bytes)
    }
  }

  property("Iso.isoStringToColl") {
    forAll() { (bytes: Coll[Byte]) =>
      roundtrip(Iso.isoStringToColl)(bytes)
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

  property("Iso.avlTree") {
    forAll { (c: sigma.AvlTree) =>
      roundtrip(AvlTree.isoAvlTree)(c)
    }
  }

  property("Iso.isoHeader") {
    forAll { (c: sigma.Header) =>
      roundtrip(Isos.isoHeader)(c)
    }
  }

  property("Iso.isoPreHeader") {
    forAll { (c: sigma.PreHeader) =>
      roundtrip(Isos.isoPreHeader)(c)
    }
  }

  property("Iso.isoBlockchainStateContext") {
    forAll(blockchainStateContextGen) { (c: BlockchainStateContext) =>
      roundtrip(Isos.isoBlockchainStateContext)(c)
    }
  }

  property("Iso.isoContextExtension") {
    forAll { (c: ContextExtension) =>
      roundtrip(Isos.isoContextExtension)(c)
    }
  }

  property("Iso.isoProverResult") {
    forAll { (c: ProverResult) =>
      roundtrip(Isos.isoProverResult)(c)
    }
  }

  property("Iso.isoUnsignedInput") {
    forAll { (c: UnsignedInput) =>
      roundtrip(Isos.isoUnsignedInput)(c)
    }
  }

  property("Iso.isoSignedInput") {
    forAll { (c: Input) =>
      roundtrip(Isos.isoSignedInput)(c)
    }
  }

  property("Iso.isoDataInput") {
    forAll { (c: DataInput) =>
      roundtrip(Isos.isoDataInput)(c)
    }
  }

  property("Iso.isoBigInt") {
    forAll { (c: sigma.BigInt) =>
      roundtrip(Isos.isoBigInt)(c)
    }
  }

  property("Iso.isoBigIntToLong") {
    forAll { (c: Long) =>
      roundtrip(Isos.isoBigIntToLong)(c)
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
      roundtrip(Isos.isoToken)((id, amount))
    }
  }

  property("Iso.isoTokenArray") {
    forAll(ergoBoxTokens(tokensGen.sample.get)) { tokens =>
      roundtrip(Isos.isoTokenArray)(tokens)
    }
  }

  property("Iso.isoUndefOr") {
    forAll { opt: Option[Long] =>
      roundtrip(sigma.js.Isos.isoUndefOr(Iso.identityIso[Long]))(opt)
    }
  }

  property("Iso.isoNonMandatoryRegisters") {
    forAll(additionalRegistersGen) { (x: AdditionalRegisters) =>
      roundtrip(Isos.isoNonMandatoryRegisters)(x)
    }
  }

  property("Iso.isoBoxCandidate") {
    forAll { (box: ErgoBoxCandidate) =>
      roundtrip(Isos.isoBoxCandidate)(box)
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

  property("Iso.isoUnsignedTransaction") {
    forAll { (tx: UnsignedErgoLikeTransaction) =>
      roundtrip(Isos.isoUnsignedTransaction)(tx)
    }
  }

  property("Iso.isoSignedTransaction") {
    forAll { (tx: ErgoLikeTransaction) =>
      roundtrip(Isos.isoSignedTransaction)(tx)
    }
  }
}
