package sigmastate.utxo.examples

import java.security.SecureRandom

import com.google.common.primitives.Longs
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Lookup}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate._
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.CryptoConstants
import org.ergoplatform.ErgoBox._
import org.ergoplatform._
import sigmastate.utxo._


class OracleExamplesSpecification extends SigmaTestingCommons {

  /**
    *
    * An oracle example.
    *
    * A trusted weather station is publishing temperature data on blockchain.
    * Alice and Bob are making a contract based on the data:
    * they have locked coins in such way that if output from the station shows that
    * temperature announced by the oracle is > 15 degrees, money are going to Alice, otherwise to Bob.
    *
    * We consider that for validating transaction only limited number of last headers and the spending transaction
    * should be enough, in addition to outputs being spent by the transaction. Thus there is no need for knowledge
    * of an arbitrary output. To show the coin of the weather service in the spending transaction, outputs from Alice
    * and Bob are referencing to the coin by using Merkle proofs against UTXO set root hash of the latest known block.
    *
    * A tricky moment is how Alice and Bob can be sure that a coin is indeed created by the service, having just the
    * coin (and also service's public key `x = g^w`, where service's secret w is not known.
    *
    *
    * For that, we consider that the service creates a coin with registers of following semantics (R0 & R1 are standard):
    *
    * R1 - coin amount
    * R2 - protecting script, which is the pubkey of the service, `x =  g^w`
    * R3 - temperature data, number
    * R4 - `a = g^r`, where r is secret random nonce
    * R5 - z = r + ew mod q
    * R6 - timestamp
    *
    * Then Alice and Bob are requiring from the coin that the following equation holds:
    * (`g^z = a * x^e`, where e = hash(R3 ++ R6)
    *
    * Thus Alice, for example, is created a coin with the following statement (we skip timeouts for simplicity):
    * "the coin is spendable by presenting a proof of Alice's private key knowledge if against UTXO set root hash for
    * the last known block there is a coin along with a Merkle proof, for which following requirements hold:
    * R2 = dlog(x) /\ g^(R5) = R4 * x^(hash(R3 ++ R6)) /\ (R3) > 15 . Similarly, the coin is spendable by a proof of
    * knowledge of the Bob's private key if all the same conditions are met but (R3) <= 15.".
    *
    * The Bob can create a box with the same guarding conditions. However, if Alice's box is already in the state, then
    * Bob can stick to it by using the trick from the "along with a brother" test.
    *
    *
    */
  property("oracle example") {
    val oracle = new ErgoLikeProvingInterpreter
    val aliceTemplate = new ErgoLikeProvingInterpreter
    val bob = new ErgoLikeProvingInterpreter

    val verifier = new ErgoLikeInterpreter

    val oraclePrivKey = oracle.dlogSecrets.head
    val oraclePubKey = oraclePrivKey.publicImage

    val alicePubKey = aliceTemplate.dlogSecrets.head.publicImage
    val bobPubKey = bob.dlogSecrets.head.publicImage

    val group = CryptoConstants.dlogGroup

    val temperature: Long = 18

    val r = BigInt.apply(128, new SecureRandom()) //128 bits random number
    val a = group.exponentiate(group.generator, r.bigInteger)

    val ts = System.currentTimeMillis()

    val e = BigInt(1, Blake2b256.hash(Longs.toByteArray(temperature) ++ Longs.toByteArray(ts)))

    val z = (r + e.bigInteger.multiply(oraclePrivKey.w)).mod(group.order).bigInteger // todo : check

    val oracleBox = ErgoBox(
      value = 1L,
      proposition = oraclePubKey,
      additionalRegisters = Map(
        R3 -> LongConstant(temperature),
        R4 -> GroupElementConstant(a),
        R5 -> BigIntConstant(z),
        R6 -> LongConstant(ts)),
      boxId = 1
    )

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)

    avlProver.performOneOperation(Insert(ADKey @@ oracleBox.id, ADValue @@ oracleBox.bytes))
    avlProver.generateProof()

    val lastBlockUtxoDigest = avlProver.digest

    val treeData = new AvlTreeData(lastBlockUtxoDigest, 32, None)

    def extract[T <: SType](Rn: RegisterIdentifier)(implicit tT: T) = ExtractRegisterAs[T](TaggedBox(22: Byte), Rn)

    def withinTimeframe(sinceHeight: Int,
                        timeoutHeight: Int,
                        fallback: Value[SBoolean.type])(script: Value[SBoolean.type]) =
      OR(AND(GE(Height, LongConstant(sinceHeight)), LT(Height, LongConstant(timeoutHeight)), script),
        AND(GE(Height, LongConstant(timeoutHeight)), fallback))

    val contractLogic = OR(AND(GT(extract[SLong.type](R3), LongConstant(15)), alicePubKey),
      AND(LE(extract[SLong.type](R3), LongConstant(15)), bobPubKey))

    val oracleProp = AND(IsMember(LastBlockUtxoRootHash, ExtractId(TaggedBox(22: Byte)), TaggedByteArray(23: Byte)),
      EQ(extract[SByteArray](R1), ByteArrayConstant(oraclePubKey.bytes)),
      EQ(Exponentiate(GroupGenerator, extract[SBigInt.type](R5)),
        MultiplyGroup(extract[SGroupElement.type](R4),
          Exponentiate(oraclePubKey.value,
            ByteArrayToBigInt(CalcBlake2b256(
              Append(LongToByteArray(extract[SLong.type](R3)), LongToByteArray(extract[SLong.type](R6)))))))
      ),
      contractLogic)

    avlProver.performOneOperation(Lookup(ADKey @@ oracleBox.id))
    val proof = avlProver.generateProof()

    val newBox1 = ErgoBox(20, alicePubKey, boxId = 2)
    val newBoxes = IndexedSeq(newBox1)
    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val sinceHeight = 40
    val timeout = 60

    val propAlice = withinTimeframe(sinceHeight, timeout, alicePubKey)(oracleProp)

    val sAlice = ErgoBox(10, propAlice, Map(), boxId = 3)

    //"along with a brother" script
    val propAlong = AND(
      EQ(SizeOf(Inputs), LongConstant(2)),
      EQ(ExtractId(ByIndex(Inputs, 0)), ByteArrayConstant(sAlice.id)))
    val propBob = withinTimeframe(sinceHeight, timeout, bobPubKey)(propAlong)
    val sBob = ErgoBox(10, propBob, Map(), boxId = 4)

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = treeData,
      boxesToSpend = IndexedSeq(sAlice, sBob),
      spendingTransaction,
      self = null)

    val alice = aliceTemplate
      .withContextExtender(22: Byte, BoxConstant(oracleBox))
      .withContextExtender(23: Byte, ByteArrayConstant(proof))
    val prA = alice.prove(propAlice, ctx, fakeMessage).get

    val prB = bob.prove(propBob, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(prA.extension)
    verifier.verify(propAlice, ctxv, prA, fakeMessage).get._1 shouldBe true

    verifier.verify(propBob, ctx, prB, fakeMessage).get._1 shouldBe true

    //todo: check timing conditions - write tests for height  < 40 and >= 60
  }


  /**
    * In previous example, Alice and Bob can use the same box with temperature written into multiple times (possibly,
    * in on one block). Costs for a prover are high though.
    *
    * In the example below we consider an alternative approach with one-time oracle box. An oracle creates a box with
    * temperature written by request, and its only spendable by a transaction which is using Allce's and Bob's boxes.
    * Protection is similar to "along with a brother" example.
    *
    * As oracle is creating the box with the data on request, it can also participate in a spending transaction.
    * Heavyweight authentication from the previous example is not needed then.
    *
    */
  property("lightweight oracle example") {
    val oracle = new ErgoLikeProvingInterpreter
    val alice = new ErgoLikeProvingInterpreter
    val bob = new ErgoLikeProvingInterpreter

    val verifier = new ErgoLikeInterpreter

    val oraclePrivKey = oracle.dlogSecrets.head
    val oraclePubKey = oraclePrivKey.publicImage

    val alicePubKey = alice.dlogSecrets.head.publicImage
    val bobPubKey = bob.dlogSecrets.head.publicImage

    val temperature: Long = 18

    val oracleBox = ErgoBox(
      value = 1L,
      proposition = oraclePubKey,
      additionalRegisters = Map(R3 -> LongConstant(temperature))
    )

    val contractLogic = OR(AND(GT(ExtractRegisterAs[SLong.type](ByIndex(Inputs, 0), R3), LongConstant(15)), alicePubKey),
      AND(LE(ExtractRegisterAs[SLong.type](ByIndex(Inputs, 0), R3), LongConstant(15)), bobPubKey))

    val prop = AND(EQ(SizeOf(Inputs), LongConstant(3)),
      EQ(ExtractScriptBytes(ByIndex(Inputs, 0)), ByteArrayConstant(oraclePubKey.bytes)),
      contractLogic
    )

    val sOracle = oracleBox
    val sAlice = ErgoBox(10, prop, Map())
    val sBob = ErgoBox(10, prop, Map())

    val newBox1 = ErgoBox(20, alicePubKey)
    val newBoxes = IndexedSeq(newBox1)
    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(sOracle, sAlice, sBob),
      spendingTransaction,
      self = null)

    val prA = alice.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prA, fakeMessage).get._1 shouldBe true
  }
}
