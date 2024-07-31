package org.ergoplatform.sdk.js

import io.circe.parser.parse
import org.ergoplatform.ErgoBox.{AdditionalRegisters, BoxId, TokenId}
import org.ergoplatform._
import org.ergoplatform.sdk.{ExtendedInputBox, JsonCodecs}
import org.ergoplatform.sdk.wallet.protocol.context.BlockchainStateContext
import org.scalacheck.Arbitrary
import sigma.ast.{Constant, SType}
import sigma.data.Iso
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.js.AvlTree
import sigma.{Coll, GroupElement}
import sigma.data.js.{Isos => DataIsos}

import scala.scalajs.js

class IsosSpec extends IsosSpecBase with sdk.generators.ObjectGenerators {

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
      roundtrip(DataIsos.isoStringToGroupElement)(bytes)
    }
  }

  property("Iso.isoBoxId") {
    forAll(boxIdGen) { (id: BoxId) =>
      roundtrip(DataIsos.isoBoxId)(id)
    }
  }

  property("Iso.isoHexStringToConstant") {
    forAll(constantGen, MinSuccessful(100)) { (c: Constant[SType]) =>
      roundtrip(DataIsos.isoHexStringToConstant)(c)
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

  property("Iso.isoHeader - test vector") {
    // valid header from Ergo blockchain
    val headerJson =
      """
        |{
        |  "extensionId" : "00cce45975d87414e8bdd8146bc88815be59cd9fe37a125b5021101e05675a18",
        |  "votes" : "000000",
        |  "timestamp" : 4928911477310178288,
        |  "size" : 223,
        |  "unparsedBytes" : "",
        |  "stateRoot" : {
        |      "digest" : "5c8c00b8403d3701557181c8df800001b6d5009e2201c6ff807d71808c00019780",
        |      "treeFlags" : "0",
        |      "keyLength" : "32"
        |  },
        |  "height" : 614400,
        |  "nBits" : 37748736,
        |  "version" : 2,
        |  "id" : "5603a937ec1988220fc44fb5022fb82d5565b961f005ebb55d85bd5a9e6f801f",
        |  "adProofsRoot" : "5d3f80dcff7f5e7f59007294c180808d0158d1ff6ba10000f901c7f0ef87dcff",
        |  "transactionsRoot" : "f17fffacb6ff7f7f1180d2ff7f1e24ffffe1ff937f807f0797b9ff6ebdae007e",
        |  "extensionRoot" : "1480887f80007f4b01cf7f013ff1ffff564a0000b9a54f00770e807f41ff88c0",
        |  "minerPk" : "03bedaee069ff4829500b3c07c4d5fe6b3ea3d3bf76c5c28c1d4dcdb1bed0ade0c",
        |  "powOnetimePk" : "0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798",
        |  "powNonce" : "0000000000003105",
        |  "powDistance" : 0,
        |  "adProofsId" : "dec129290a763f4de41f04e87e2b661dd59758af6bdd00dd51f5d97c3a8cb9b5",
        |  "transactionsId" : "eba1dd82cf51147232e09c1f72b37c554c30f63274d5093bff36849a83472a42",
        |  "parentId" : "ac2101807f0000ca01ff0119db227f202201007f62000177a080005d440896d0"
        |}
        |""".stripMargin

    object JsonCodecs extends JsonCodecs
    val c = JsonCodecs.headerDecoder.decodeJson(parse(headerJson).toOption.get).toOption.get

    roundtrip(Isos.isoHeader)(c)

    val jh = Isos.isoHeader.from(c)
    Isos.isoHeader.to(jh).serializeWithoutPoW shouldBe c.serializeWithoutPoW
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
      roundtrip(sigma.js.Isos.isoBigInt)(c)
    }
  }

  property("Iso.isoBigIntToLong") {
    forAll { (c: Long) =>
      roundtrip(sigma.js.Isos.isoBigIntToLong)(c)
    }
  }

  property("Iso.isoBigIntToLong - test vector") {
    val l = 4928911477310178288L
    val js =  sigma.js.Isos.isoBigIntToLong.from(l)
    l.toString shouldBe js.toString()
  }

  property("Iso.isoAmount") {
    forAll { (c: Long) =>
      roundtrip(DataIsos.isoAmount)(c)
      DataIsos.isoAmount.to(js.BigInt(c.toString)) shouldBe c
    }
  }

  property("Iso.isoToken") {
    forAll(tokenIdGen, Arbitrary.arbLong.arbitrary) { (id: TokenId, amount: Long) =>
      roundtrip(DataIsos.isoToken)((id, amount))
    }
  }

  property("Iso.isoTokenArray") {
    forAll(ergoBoxTokens(tokensGen.sample.get)) { tokens =>
      roundtrip(DataIsos.isoTokenArray)(tokens)
    }
  }

  property("Iso.isoUndefOr") {
    forAll { opt: Option[Long] =>
      roundtrip(sigma.js.Isos.isoUndefOr(Iso.identityIso[Long]))(opt)
    }
  }

  property("Iso.isoNonMandatoryRegisters") {
    forAll(additionalRegistersGen) { (x: AdditionalRegisters) =>
      roundtrip(DataIsos.isoNonMandatoryRegisters)(x)
    }
  }

  property("Iso.isoBoxCandidate") {
    forAll { (box: ErgoBoxCandidate) =>
      roundtrip(DataIsos.isoBoxCandidate)(box)
    }
  }

  property("Iso.isoBox") {
    forAll { (b: ErgoBox) =>
      roundtrip(sigma.js.Box.isoBox)(b)
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

  property("Iso.isoContractTemplate") {
    forAll(contractTemplateGen) { (tx: sdk.ContractTemplate) =>
      roundtrip(ContractTemplate.isoToSdk)(tx)
    }
  }

}
