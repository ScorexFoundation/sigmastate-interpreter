package sigmastate.utxo

import org.ergoplatform._
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{ByteArrayConstant, IntConstant, LongConstant, TrueLeaf, Value}
import sigmastate._
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}
import sigmastate.serialization.ValueSerializer

class AssetsSpecification extends SigmaTestingCommons{

  property("buy request"){
    val prover = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter

    val tokenId = Blake2b256("token1")

    val pubkey = prover.dlogSecrets.head.publicImage
    val pubKeyBytes = ValueSerializer.serialize(pubkey)

    val tokenTypeEv = SCollection(STuple(SCollection.SByteArray, SLong))

    def extractToken(box: Value[SBox.type]) = ByIndex(ExtractRegisterAs(box, ErgoBox.TokensRegId)(tokenTypeEv), 0)
    def extractTokenId(box: Value[SBox.type]) = SelectField(extractToken(box), 1).asInstanceOf[Value[SCollection.SByteArray]]

    val rightProtection = EQ(ExtractScriptBytes(ByIndex(Outputs, IntConstant.Zero)), ByteArrayConstant(pubKeyBytes))

    val prop = AND(
      EQ(extractTokenId(ByIndex(Outputs, 0)), ByteArrayConstant(tokenId)),
      rightProtection,
      GE(ExtractAmount(ByIndex(Outputs, 0)), LongConstant(1))
    )

    val newBox1 = ErgoBox(1, pubkey, Seq(tokenId -> 50))
    val newBoxes = IndexedSeq(newBox1)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    //todo: put prop instead of TrueLeaf here
    val s = ErgoBox(1, TrueLeaf)

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = s)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true
  }

}
