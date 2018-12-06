package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox.R4
import org.ergoplatform._
import scorex.crypto.hash.Blake2b256
import sigmastate.SCollection.SByteArray
import sigmastate.Values.{ByteArrayConstant, IntConstant, LongConstant, ShortConstant, Value}
import sigmastate._
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.ContextExtension
import sigmastate.lang.Terms._
import sigmastate.utxo._

/**
  * An example of an atomic ergo <=> asset exchange.
  * Let's assume that Alice is willing to buy 60 assets of type "token1" for 100 ergo coins, and Bob
  * is willing to be a counterparty in this deal.
  *
  * Alice is creating a box of 100 coins protected by the script "I demand a spending transaction to create a box
  * which is protected by my public key pubkey_A and contains at least 60 assets of type "token1" and also 1 ergo coin"
  * (the last condition ensures that the box is easily spendable).
  *
  * Similarly, Bob is creating a box protected by a script like "I demand a spending transaction to create a box
  * which is protected by my public ket pubkey_B and contains at least 100 ergo tokens".
  *
  * (Please note that we are skipping some practically important details, for example, order cancellation conditions).
  *
  * Please note that once both box are on the blockchain, a correct exchange transaction could be created and
  * posted by anyone.
  *
  * Please note that more complex protocols could be build on top of the atomic exchange. For example, transactions
  * creating both boxes could be sent off-chain to a matching service, and be posted on the blockchain along with
  * the exchange transaction.
  *
  * //todo: make an example of multiple orders being matched
  */
class AssetsAtomicExchangeSpecification extends SigmaTestingCommons {

  /**
    * A simpler example with single-chain atomic exchange contracts.
    */
  property("atomic exchange") {
    val tokenBuyer = new ErgoLikeTestProvingInterpreter
    val tokenSeller = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val tokenId = Blake2b256("token1")
    val deadline = 70L
    val tokenBuyerKey = tokenBuyer.dlogSecrets.head.publicImage
    val tokenSellerKey = tokenBuyer.dlogSecrets.head.publicImage

    val buyerKeyBytes = tokenBuyerKey.bytes
    val sellerKeyBytes = tokenSellerKey.bytes

    def extractToken(box: Value[SBox.type]) = ByIndex(
      ExtractRegisterAs(box, ErgoBox.TokensRegId)(ErgoBox.STokensRegType).get, 0)

    def extractTokenId(box: Value[SBox.type]) =
      SelectField(extractToken(box), 1).asInstanceOf[Value[SByteArray]]

    def extractTokenAmount(box: Value[SBox.type]) =
      SelectField(extractToken(box), 2).asInstanceOf[Value[SLong.type]]

    val rightProtectionBuyer =
      EQ(ExtractScriptBytes(ByIndex(Outputs, IntConstant.Zero)), ByteArrayConstant(buyerKeyBytes))

    val rightProtectionSeller =
      EQ(ExtractScriptBytes(ByIndex(Outputs, IntConstant.One)), ByteArrayConstant(sellerKeyBytes))

    val buyerProp = OR(
      AND(GT(Height, deadline), tokenSellerKey),
      AND(
        EQ(extractTokenId(ByIndex(Outputs, IntConstant.Zero)), ByteArrayConstant(tokenId)),
        GE(extractTokenAmount(ByIndex(Outputs, IntConstant.Zero)), LongConstant(60)),
        rightProtectionBuyer,
        EQ(OptionGet(ExtractRegisterAs[SByteArray](ByIndex(Outputs, IntConstant.Zero), R4)), ExtractId(Self))
      )
    )

    val buyerEnv = Map("pkA" -> tokenBuyerKey, "deadline" -> deadline, "token1" -> tokenId)
    val altBuyerProp = compile(buyerEnv,
      """(HEIGHT > deadline && pkA) || {
        |  val tokenData = OUTPUTS(0).R2[Array[(Array[Byte], Long)]].get(0)
        |  allOf(Array(
        |      tokenData._1 == token1,
        |      tokenData._2 >= 60L,
        |      OUTPUTS(0).propositionBytes == pkA.propBytes,
        |      OUTPUTS(0).R4[Array[Byte]].get == SELF.id
        |  ))
        |}
      """.stripMargin).asBoolValue
   altBuyerProp shouldBe buyerProp

    val sellerProp = OR(
      AND(GT(Height, deadline), tokenSellerKey),
      AND(
        GE(ExtractAmount(ByIndex(Outputs, IntConstant.One)), LongConstant(100)),
        EQ(OptionGet(ExtractRegisterAs[SByteArray](ByIndex(Outputs, IntConstant.One), R4)), ExtractId(Self)),
        rightProtectionSeller
      )
    )
    val sellerEnv = Map("pkB" -> tokenSellerKey, "deadline" -> deadline)
    val altSellerProp = compile(sellerEnv,
      """ (HEIGHT > deadline && pkB) ||
        | allOf(Array(
        |        OUTPUTS(1).value >= 100,
        |        OUTPUTS(1).R4[Array[Byte]].get == SELF.id,
        |        OUTPUTS(1).propositionBytes == pkB.propBytes
        | ))
      """.stripMargin).asBoolValue

    sellerProp shouldBe altSellerProp

    //tx inputs
    val input0 = ErgoBox(100, buyerProp, 0)
    val input1 = ErgoBox(1, sellerProp, 0, Seq(tokenId -> 60))

    //tx outputs
    val newBox1 = ErgoBox(1, tokenBuyerKey, 0, Seq(tokenId -> 60), Map(R4 -> ByteArrayConstant(input0.id)))
    val newBox2 = ErgoBox(100, tokenSellerKey, 0, Seq(), Map(R4 -> ByteArrayConstant(input1.id)))
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val buyerCtx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(input0, input1),
      spendingTransaction,
      self = input0
    )

    //Though we use separate provers below, both inputs do not contain any secrets, thus
    //a spending transaction could be created and posted by anyone.
    val pr = tokenBuyer.prove(buyerProp, buyerCtx, fakeMessage).get
    verifier.verify(buyerProp, buyerCtx, pr, fakeMessage).get._1 shouldBe true

    val sellerCtx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(input0, input1),
      spendingTransaction,
      self = input1
    )

    val pr2 = tokenSeller.prove(sellerProp, sellerCtx, fakeMessage).get

    // All requirements satisfied
    verifier.verify(sellerProp, sellerCtx, pr2, fakeMessage).get._1 shouldBe true

    println("total cost: " + (buyerProp.cost(buyerCtx) + sellerProp.cost(sellerCtx)))

    val inputWithSmallerValue = ErgoBox(1, sellerProp, 0, Seq(tokenId -> 59))
    val sellerCtxInvalid = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(input0, inputWithSmallerValue),
      spendingTransaction,
      self = inputWithSmallerValue
    )

    // Does not satisfy requirement `tokenData._2 >= 60L` in buyer's script.
    verifier.verify(sellerProp, sellerCtxInvalid, pr2, fakeMessage).get._1 shouldBe false
  }

  /**
    * An example with order contracts which could be only partially filled
    */
  property("partial filling") {
    val tokenBuyer = new ErgoLikeTestProvingInterpreter
    val tokenSeller = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val tokenId = Blake2b256("token1")
    val deadline = 70L
    val tokenBuyerKey = tokenBuyer.dlogSecrets.head.publicImage
    val tokenSellerKey = tokenBuyer.dlogSecrets.head.publicImage

    val buyerEnv = Map("pkA" -> tokenBuyerKey, "deadline" -> deadline, "token1" -> tokenId)

    //the contract assumes no tokens in the input box
    val buyerProp = compile(buyerEnv,
      """(HEIGHT > deadline && pkA) || {
        |
        |  val outIdx = getVar[Short](127).get
        |  val out = OUTPUTS(outIdx)
        |  val tokenData = out.R2[Array[(Array[Byte], Long)]].get(0)
        |  val tokenId = tokenData._1
        |  val tokenValue = tokenData._2
        |  val outValue = out.value
        |  val price = 500
        |  val minimalRemainingAmount = 500
        |
        |  allOf(Array(
        |      tokenId == token1,
        |      tokenValue >= 1,
        |      (SELF.value - outValue) <= tokenValue * price,
        |      out.R4[Array[Byte]].get == SELF.id,
        |      out.propositionBytes == SELF.propositionBytes ||
        |          (outValue <= minimalRemainingAmount && out.propositionBytes == pkA.propBytes)
        |  ))
        |}
      """.stripMargin).asBoolValue

    val sellerEnv = Map("pkB" -> tokenSellerKey, "deadline" -> deadline)
    val sellerProp = compile(sellerEnv,
      """ (HEIGHT > deadline && pkB) || {
        |   val outIdx = getVar[Short](127).get
        |   val out = OUTPUTS(outIdx)
        |
        |   val tokenData = out.R2[Array[(Array[Byte], Long)]].get(0)
        |   val selfTokensDataOpt = SELF.R2[Array[(Array[Byte], Long)]]
        |   val tokenValue = tokenData._2
        |   val selfTokenValue = if (selfTokensDataOpt.isDefined) selfTokensDataOpt.get(0)._2 else 0L
        |
        |   val selfValue = SELF.value
        |   val outValue = out.value
        |
        |   val sold = selfTokenValue - tokenValue
        |
        |   val price = 495
        |
        |   allOf(Array(
        |        sold >= 1,
        |        (outValue - selfValue) >= sold * price,
        |        out.R4[Array[Byte]].get == SELF.id,
        |        out.propositionBytes == SELF.propositionBytes ||
        |            (tokenValue == 0 && out.propositionBytes == pkB.propBytes)
        |   ))
        | }
      """.stripMargin).asBoolValue

    //tx inputs
    val input0 = ErgoBox(10000, buyerProp, 0)
    val input1 = ErgoBox(0, sellerProp, 0, Seq(tokenId -> 60))

    //tx outputs at 1st round
    val buyerBoxRound1 = ErgoBox(5000, buyerProp, 0, Seq(tokenId -> 10), Map(R4 -> ByteArrayConstant(input0.id)))
    val sellerBoxRound1 = ErgoBox(4950, sellerProp, 0, Seq(tokenId -> 50), Map(R4 -> ByteArrayConstant(input1.id)))
    val newBoxesRound1 = IndexedSeq(buyerBoxRound1, sellerBoxRound1)

    //tx outputs at 2nd round
    val buyerBoxRound2 = ErgoBox(500, tokenBuyerKey, 1, Seq(tokenId -> 19), Map(R4 -> ByteArrayConstant(buyerBoxRound1.id)))
    val sellerBoxRound2 = ErgoBox(29700, tokenSellerKey, 1, Seq(), Map(R4 -> ByteArrayConstant(sellerBoxRound1.id)))
    val newBoxesRound2 = IndexedSeq(buyerBoxRound2, sellerBoxRound2)

    val spendingTransactionRound1 = ErgoLikeTransaction(IndexedSeq(), newBoxesRound1)
    val spendingTransactionRound2 = ErgoLikeTransaction(IndexedSeq(), newBoxesRound2)

    val buyerCtxRound1 = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(input0, input1),
      spendingTransactionRound1,
      self = input0,
      extension = ContextExtension(Map(Byte.MaxValue -> ShortConstant(0))))

    //Though we use separate provers below, both inputs do not contain any secrets, thus
    //a spending transaction could be created and posted by anyone.
    val pr1R1 = tokenBuyer.withContextExtender(Byte.MaxValue, ShortConstant(0)).prove(buyerProp, buyerCtxRound1, fakeMessage).get
    verifier.verify(buyerProp, buyerCtxRound1, pr1R1, fakeMessage).get._1 shouldBe true

    val sellerCtxRound1 = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(input0, input1),
      spendingTransactionRound1,
      self = input1,
      extension = ContextExtension(Map(Byte.MaxValue -> ShortConstant(1))))

    val pr2 = tokenSeller.withContextExtender(Byte.MaxValue, ShortConstant(1)).prove(sellerProp, sellerCtxRound1, fakeMessage).get
    verifier.verify(sellerProp, sellerCtxRound1, pr2, fakeMessage).get._1 shouldBe true

    println("total cost: " + (buyerProp.cost(buyerCtxRound1) + sellerProp.cost(sellerCtxRound1)))

    val buyerCtxRound2 = ErgoLikeContext(
      currentHeight = 51,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = newBoxesRound1,
      spendingTransactionRound2,
      self = buyerBoxRound1,
      extension = ContextExtension(Map(Byte.MaxValue -> ShortConstant(0))))

    val pr1R2 = tokenBuyer.withContextExtender(Byte.MaxValue, ShortConstant(0)).prove(buyerProp, buyerCtxRound2, fakeMessage).get
    verifier.verify(buyerProp, buyerCtxRound2, pr1R2, fakeMessage).get._1 shouldBe true

    val sellerCtxRound2 = ErgoLikeContext(
      currentHeight = 51,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = newBoxesRound1,
      spendingTransactionRound2,
      self = sellerBoxRound1,
      extension = ContextExtension(Map(Byte.MaxValue -> ShortConstant(1))))

    //val pr2R2 = tokenSeller.withContextExtender(Byte.MaxValue, ShortConstant(1)).prove(sellerProp, sellerCtxRound2, fakeMessage).get
    //verifier.verify(sellerProp, sellerCtxRound2, pr2R2, fakeMessage).get._1 shouldBe true
  }

}
