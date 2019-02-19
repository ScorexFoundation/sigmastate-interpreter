package sigmastate.utxo.examples

import sigmastate.helpers.SigmaTestingCommons
import org.ergoplatform.dsl.ContractSyntax.Token
import org.ergoplatform.ErgoBox.R4
import org.ergoplatform.dsl.ErgoContractSpec
import sigmastate.SCollection.SByteArray
import special.collection.Coll
import scorex.crypto.hash.Blake2b256
import sigmastate.utxo._
import org.ergoplatform.{Height, Outputs, ErgoBox, Self}
import sigmastate.Values.{LongConstant, BlockValue, Value, ByteArrayConstant, ValDef, ValUse}
import sigmastate._
import sigmastate.lang.Terms.ValueOps

class AssetsAtomicExchangeErgoTests extends SigmaTestingCommons { suite =>
  lazy val spec = ErgoContractSpec(suite)(new TestingIRContext)
  private lazy val tokenId: Coll[Byte] = spec.Coll(Blake2b256("token1"))
  lazy val buyer = spec.ProvingParty("Alice")
  lazy val seller = spec.ProvingParty("Bob")
  val ergAmt = 100
  val tAmt = 60
  val buyerBoxId: Coll[Byte] = spec.Coll(Blake2b256("BBox"))
  val sellerBoxId: Coll[Byte] = spec.Coll(Blake2b256("SBox"))

  property("atomic exchange spec") {
    val contract = new AssetsAtomicExchange[spec.type](70, tokenId, buyer, seller)(spec) {
      import spec._

      def extractToken(box: Value[SBox.type]) = ByIndex(
        ExtractRegisterAs(box, ErgoBox.TokensRegId)(ErgoBox.STokensRegType).get, 0)

      val expectedBuyerTree = BlockValue(
        Vector(
          ValDef(1, ByIndex(Outputs, 0)),
          // token
          ValDef(2, extractToken(ValUse(1, SBox)))
        ),
        SigmaOr(List(
          SigmaAnd(List(
            GT(Height, deadline).toSigmaProp,
            tokenBuyer.pubKey.toTreeData.asSigmaProp
          )),
          AND(
            // extract toked id
            EQ(SelectField(ValUse(2, STuple(SByteArray, SLong)), 1), ByteArrayConstant(tokenId.toArray)),
            // extract token amount
            GE(SelectField(ValUse(2, STuple(SByteArray, SLong)), 2), LongConstant(60)),
            // right protection buyer
            EQ(ExtractScriptBytes(ValUse(1, SBox)), tokenBuyer.pubKey.toTreeData.asSigmaProp.propBytes),
            EQ(ExtractRegisterAs(ValUse(1, SBox), R4, SOption(SCollection(SByte))).get, ExtractId(Self))
          ).toSigmaProp
        ))
      ).asBoolValue
      buyerProp.ergoTree.proposition shouldBe expectedBuyerTree
    }
    import contract.spec._

    // ARRANGE
//    val startBlock = getBlock(50)
//    val txs = startBlock.getTransactions()
//    val buyerBox = txs(0).outputs(0)
//    val sellerBox = getBoxesByParty(seller).collectFirst { case b if b.value > ergAmt => b }.get
    val buyerBox = getBoxById(buyerBoxId)
    val sellerBox = getBoxById(sellerBoxId)
    val txCtx = newTransactionContext
    // ACT


    // start exchange protocol
    val (ergHolder, tokenHolder) = contract.startExchange(txCtx.block, buyerBox, sellerBox, 100, Token(contract.tokenId, 60))

    // setup spending transaction
    val (buyerTokens, sellerErgs) = contract.finishExchange(txCtx.block, ergHolder, tokenHolder)

    // ASSERT
    val input0 = buyerTokens.tx.inputs(0)
//    val res = input0.runDsl()
//    res shouldBe CSigmaProp(TrivialProp.TrueProp)

    val buyerProof = contract.tokenBuyer.prove(input0).get

    txCtx.attachProof(input0 -> buyerProof)
    try {
      txCtx.submit()
    } catch {
      case t: Throwable =>
    }
  }
}
