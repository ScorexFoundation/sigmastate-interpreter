package sigmastate.utxo

import org.ergoplatform._
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{ByteArrayConstant, IntConstant, LongConstant, TrueLeaf, Value}
import sigmastate._
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}
import sigmastate.serialization.ValueSerializer

class AssetsSpecification extends SigmaTestingCommons {

  property("atomic request") {
    val tokenBuyer = new ErgoLikeProvingInterpreter
    val tokenSeller = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter

    val tokenId = Blake2b256("token1")

    val tokenBuyerKey = tokenBuyer.dlogSecrets.head.publicImage
    val tokenSellerKey = tokenBuyer.dlogSecrets.head.publicImage

    val buyerKeyBytes = ValueSerializer.serialize(tokenBuyerKey)
    val sellerKeyBytes = ValueSerializer.serialize(tokenSellerKey)

    val tokenTypeEv = SCollection(STuple(SCollection.SByteArray, SLong))

    def extractToken(box: Value[SBox.type]) = ByIndex(ExtractRegisterAs(box, ErgoBox.TokensRegId)(tokenTypeEv), 0)

    def extractTokenId(box: Value[SBox.type]) =
      SelectField(extractToken(box), 1).asInstanceOf[Value[SCollection.SByteArray]]

    def extractTokenAmount(box: Value[SBox.type]) =
      SelectField(extractToken(box), 2).asInstanceOf[Value[SLong.type]]

    val rightProtectionBuyer =
      EQ(ExtractScriptBytes(ByIndex(Outputs, IntConstant.Zero)), ByteArrayConstant(buyerKeyBytes))

    val rightProtectionSeller =
      EQ(ExtractScriptBytes(ByIndex(Outputs, IntConstant.One)), ByteArrayConstant(sellerKeyBytes))

    val buyerProp = AND(
      EQ(extractTokenId(ByIndex(Outputs, IntConstant.Zero)), ByteArrayConstant(tokenId)),
      GE(extractTokenAmount(ByIndex(Outputs, IntConstant.Zero)), LongConstant(60)),
      rightProtectionBuyer,
      GE(ExtractAmount(ByIndex(Outputs, IntConstant.Zero)), LongConstant(1))
    )

    val sellerProp = AND(
      rightProtectionSeller,
      GE(ExtractAmount(ByIndex(Outputs, IntConstant.One)), LongConstant(100))
    )

    val newBox1 = ErgoBox(1, tokenBuyerKey, Seq(tokenId -> 60))
    val newBox2 = ErgoBox(100, tokenSellerKey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    //todo: put buyerProp instead of TrueLeaf here
    val input1 = ErgoBox(100, TrueLeaf)

    //todo: put sellerProp instead of TrueLeaf here
    val input2 = ErgoBox(1, TrueLeaf)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val buyerCtx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(input1, input2),
      spendingTransaction,
      self = input1)


    //Though we use separate provers below, both inputs do not contain any secrets, thus
    //a spending transaction could be created and posted by anyone.
    val pr = tokenBuyer.prove(buyerProp, buyerCtx, fakeMessage).get
    verifier.verify(buyerProp, buyerCtx, pr, fakeMessage).get._1 shouldBe true

    val sellerCtx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(input1, input2),
      spendingTransaction,
      self = input2)

    val pr2 = tokenSeller.prove(sellerProp, sellerCtx, fakeMessage).get
    verifier.verify(sellerProp, sellerCtx, pr2, fakeMessage).get._1 shouldBe true
  }
}