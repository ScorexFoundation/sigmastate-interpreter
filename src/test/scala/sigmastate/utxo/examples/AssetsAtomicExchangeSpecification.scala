package sigmastate.utxo.examples

import org.ergoplatform._
import scorex.crypto.hash.Blake2b256
import sigmastate.SCollection.SByteArray
import sigmastate.Values.{LongConstant, SigmaPropConstant, Value, ByteArrayConstant, IntConstant}
import sigmastate._
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigmastate.utxo._
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
  implicit lazy val IR = new TestingIRContext

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
      EQ(ExtractScriptBytes(ByIndex(Outputs, IntConstant.Zero)), tokenBuyerKey.propBytes)

    val rightProtectionSeller =
      EQ(ExtractScriptBytes(ByIndex(Outputs, IntConstant.One)), tokenSellerKey.propBytes)

    val buyerProp = BinOr(
      BinAnd(GT(Height, deadline), tokenSellerKey.isValid),
      AND(
        EQ(extractTokenId(ByIndex(Outputs, IntConstant.Zero)), ByteArrayConstant(tokenId)),
        GE(extractTokenAmount(ByIndex(Outputs, IntConstant.Zero)), LongConstant(60)),
        rightProtectionBuyer,
        GE(ExtractAmount(ByIndex(Outputs, IntConstant.Zero)), LongConstant(1))
      )
    )

    //    val x = OR(ConcreteCollection(Vector(AND(ConcreteCollection(Vector(GT(Height, Constant(70, SLong)), ProveDlog(Constant((???, ???, ???, ???), SGroupElement))), SBoolean)), AND(ConcreteCollection(Vector(EQ(SelectField(Apply(ExtractRegisterAs(ByIndex(Outputs, Constant(0, SInt), None), R2, Array[(Array[SByte], SLong)], None), Vector(Constant(0, SInt))), 1), Constant(???, Array[SByte])), GE(SelectField(Apply(ExtractRegisterAs(ByIndex(Outputs, Constant(0, SInt), None), R2, Array[(Array[SByte], SLong)], None), Vector(Constant(0, SInt))), 2), Constant(60, SLong)), EQ(ExtractScriptBytes(ByIndex(Outputs, Constant(0, SInt), None)), Constant(???, Array[SByte])), GE(ExtractAmount(ByIndex(Outputs, Constant(0, SInt), None)), Constant(1, SLong))), SBoolean))), SBoolean))

    val buyerEnv = Map(
      ScriptNameProp -> "buyer",
      "pkA" -> tokenBuyerKey, "deadline" -> deadline, "token1" -> tokenId)
    val altBuyerProp = compile(buyerEnv,
      """(HEIGHT > deadline && pkA) || {
        |  val tokenData = OUTPUTS(0).R2[Array[(Array[Byte], Long)]].get(0)
        |  allOf(Array(
        |      tokenData._1 == token1,
        |      tokenData._2 >= 60L,
        |      OUTPUTS(0).propositionBytes == pkA.propBytes,
        |      OUTPUTS(0).value >= 1L
        |  ))
        |}
      """.stripMargin).asBoolValue
    altBuyerProp shouldBe buyerProp

    val sellerProp = BinOr(
      BinAnd(GT(Height, deadline), tokenSellerKey.isValid),
      AND(
        GE(ExtractAmount(ByIndex(Outputs, IntConstant.One)), LongConstant(100)),
        rightProtectionSeller
      )
    )
    val sellerEnv = Map(
      ScriptNameProp -> "seller",
      "pkB" -> tokenSellerKey, "deadline" -> deadline)
    val altSellerProp = compile(sellerEnv,
      """ (HEIGHT > deadline && pkB) ||
        | allOf(Array(
        |        OUTPUTS(1).value >= 100,
        |        OUTPUTS(1).propositionBytes == pkB.propBytes
        | ))
      """.stripMargin).asBoolValue

    altSellerProp shouldBe sellerProp

    val newBox1 = ErgoBox(1, tokenBuyerKey, Seq(tokenId -> 60))
    val newBox2 = ErgoBox(100, tokenSellerKey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val input1 = ErgoBox(100, buyerProp)

    val input2 = ErgoBox(1, sellerProp, Seq(tokenId -> 60))

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val buyerCtx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(input1, input2),
      spendingTransaction,
      self = input1)

    //Though we use separate provers below, both inputs do not contain any secrets, thus
    //a spending transaction could be created and posted by anyone.
    val pr = tokenBuyer.prove(emptyEnv + (ScriptNameProp -> "tokenBuyer_prove"), buyerProp, buyerCtx, fakeMessage).get
    verifier.verify(emptyEnv + (ScriptNameProp -> "tokenBuyer_verify"), buyerProp, buyerCtx, pr, fakeMessage).get._1 shouldBe true

    val sellerCtx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(input1, input2),
      spendingTransaction,
      self = input2)

    val pr2 = tokenSeller.prove(sellerProp, sellerCtx, fakeMessage).get
    verifier.verify(sellerProp, sellerCtx, pr2, fakeMessage).get._1 shouldBe true

    println("total cost: " + (buyerProp.cost(buyerCtx) + sellerProp.cost(sellerCtx)))
  }
}