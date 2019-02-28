package sigmastate.utxo.examples

import java.security.SecureRandom

import com.google.common.primitives.Longs
import org.ergoplatform.ErgoBox.{R4, RegisterId}
import scorex.crypto.authds.avltree.batch.{Lookup, BatchAVLProver, Insert}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Digest32, Blake2b256}
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.CryptoConstants
import org.ergoplatform._
import org.ergoplatform.dsl.ContractSyntax.Token
import org.ergoplatform.dsl.{SigmaContractSyntax, ContractSpec, TestContractSpec, StdContracts}
import sigmastate.TrivialProp.TrueProp
import sigmastate.eval.CSigmaProp
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigmastate.utxo._
import special.collection.Coll
import special.sigma.Context


class OracleExamplesSpecification extends SigmaTestingCommons { suite =>
  implicit lazy val IR = new TestingIRContext

  private val reg1 = ErgoBox.nonMandatoryRegisters(0)
  private val reg2 = ErgoBox.nonMandatoryRegisters(1)
  private val reg3 = ErgoBox.nonMandatoryRegisters(2)
  private val reg4 = ErgoBox.nonMandatoryRegisters(3)

  /**
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
    val oracle = new ErgoLikeTestProvingInterpreter
    val aliceTemplate = new ErgoLikeTestProvingInterpreter
    val bob = new ErgoLikeTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val oraclePrivKey = oracle.dlogSecrets.head
    val oraclePubImage = oraclePrivKey.publicImage
    val oraclePubKey = oraclePubImage

    val alicePubKey = aliceTemplate.dlogSecrets.head.publicImage
    val bobPubKey = bob.dlogSecrets.head.publicImage

    val group = CryptoConstants.dlogGroup

    val temperature: Long = 18

    val r = BigInt.apply(128, new SecureRandom()) //128 bits random number
    val a = group.exponentiate(group.generator, r.bigInteger)

    val ts = System.currentTimeMillis()

    // we need to fit the resulted BigInt in group order
    val reducedHashSize = 31
    val e = BigInt(1, Blake2b256.hash(Longs.toByteArray(temperature) ++ Longs.toByteArray(ts)).take(reducedHashSize))

    val z = (r + e.bigInteger.multiply(oraclePrivKey.w)).mod(group.order).bigInteger // todo : check

    val oracleBox = ErgoBox(
      value = 1L,
      ergoTree = oraclePubKey,
      creationHeight = 0,
      additionalRegisters = Map(
        reg1 -> LongConstant(temperature),
        reg2 -> GroupElementConstant(a),
        reg3 -> BigIntConstant(z),
        reg4 -> LongConstant(ts)),
      boxIndex = 1
    )

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)

    avlProver.performOneOperation(Insert(ADKey @@ oracleBox.id, ADValue @@ oracleBox.bytes))
    avlProver.generateProof()

    val lastBlockUtxoDigest = avlProver.digest

    val treeData = new AvlTreeData(lastBlockUtxoDigest, AvlTreeFlags.ReadOnly, 32, None)

    def extract[T <: SType](Rn: RegisterId)(implicit tT: T) =
      ExtractRegisterAs[T](GetVarBox(22: Byte).get, Rn)(tT).get

    def withinTimeframe(sinceHeight: Int,
                        timeoutHeight: Int,
                        fallback: Value[SBoolean.type])(script: Value[SBoolean.type]) =
      OR(AND(GE(Height, IntConstant(sinceHeight)), LT(Height, IntConstant(timeoutHeight)), script),
        AND(GE(Height, IntConstant(timeoutHeight)), fallback))

    val contractLogic = OR(AND(GT(extract[SLong.type](reg1), LongConstant(15)), alicePubKey.isProven),
      AND(LE(extract[SLong.type](reg1), LongConstant(15)), bobPubKey.isProven))

    val oracleProp = AND(
      OptionIsDefined(IR.builder.mkMethodCall(
        LastBlockUtxoRootHash, SAvlTree.getMethod,
        IndexedSeq(ExtractId(GetVarBox(22: Byte).get), GetVarByteArray(23: Byte).get)).asOption[SByteArray]),
      EQ(extract[SByteArray](ErgoBox.ScriptRegId), ByteArrayConstant(ErgoTree.fromSigmaBoolean(oraclePubKey).bytes)),
      EQ(Exponentiate(GroupGenerator, extract[SBigInt.type](reg3)),
        MultiplyGroup(extract[SGroupElement.type](reg2),
          Exponentiate(oraclePubImage.value,
            ByteArrayToBigInt(
              Slice(
                CalcBlake2b256(
                  Append(LongToByteArray(extract[SLong.type](reg1)), LongToByteArray(extract[SLong.type](reg4)))),
                IntConstant(0), IntConstant(reducedHashSize)
              )
            )
          )
        )
      ),
      contractLogic)

    avlProver.performOneOperation(Lookup(ADKey @@ oracleBox.id))
    val proof = avlProver.generateProof()

    val newBox1 = ErgoBox(20, alicePubKey, 0, boxIndex = 2)
    val newBoxes = IndexedSeq(newBox1)
    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val sinceHeight = 40
    val timeout = 60

    val propAlice = withinTimeframe(sinceHeight, timeout, alicePubKey.isProven)(oracleProp).toSigmaProp

    val sAlice = ErgoBox(10, propAlice, 0, Seq(), Map(), boxIndex = 3)

    //"along with a brother" script
    val propAlong = AND(
      EQ(SizeOf(Inputs), IntConstant(2)),
      EQ(ExtractId(ByIndex(Inputs, 0)), ByteArrayConstant(sAlice.id)))
    val propBob = withinTimeframe(sinceHeight, timeout, bobPubKey.isProven)(propAlong).toSigmaProp
    val sBob = ErgoBox(10, propBob, 0, Seq(), Map(), boxIndex = 4)

   val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = treeData,
      ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(sAlice, sBob),
      spendingTransaction,
      self = null)

    val alice = aliceTemplate
      .withContextExtender(22: Byte, BoxConstant(oracleBox))
      .withContextExtender(23: Byte, ByteArrayConstant(proof))
    val prA = alice.prove(emptyEnv + (ScriptNameProp -> "alice_prove"), propAlice, ctx, fakeMessage).fold(t => throw t, x => x)

    val prB = bob.prove(emptyEnv + (ScriptNameProp -> "bob_prove"), propBob, ctx, fakeMessage).fold(t => throw t, x => x)

    val ctxv = ctx.withExtension(prA.extension)
    verifier.verify(emptyEnv + (ScriptNameProp -> "alice_verify"), propAlice, ctxv, prA, fakeMessage).get._1 shouldBe true

    verifier.verify(emptyEnv + (ScriptNameProp -> "bob_verify"), propBob, ctx, prB, fakeMessage).get._1 shouldBe true

    //todo: check timing conditions - write tests for height  < 40 and >= 60
  }



  /**
    * In previous example, Alice and Bob can use the same box with temperature written into multiple times (possibly,
    * in one block). Costs for a prover are high though.
    *
    * In the example below we consider an alternative approach with one-time oracle box. An oracle creates a box with
    * temperature written by request, and its only spendable by a transaction which is using Allce's and Bob's boxes.
    * Protection is similar to "along with a brother" example.
    *
    * As oracle is creating the box with the data on request, it can also participate in a spending transaction.
    * Heavyweight authentication from the previous example is not needed then.
    */
  property("lightweight oracle example") {
    val oracle = new ErgoLikeTestProvingInterpreter
    val alice = new ErgoLikeTestProvingInterpreter
    val bob = new ErgoLikeTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val oraclePrivKey = oracle.dlogSecrets.head
    val oraclePubKey = oraclePrivKey.publicImage

    val alicePubKey = alice.dlogSecrets.head.publicImage
    val bobPubKey = bob.dlogSecrets.head.publicImage

    val temperature: Long = 18

    val oracleBox = ErgoBox(
      value = 1L,
      ergoTree = oraclePubKey,
      creationHeight = 0,
      additionalRegisters = Map(reg1 -> LongConstant(temperature))
    )

    val contractLogic = OR(
      AND(GT(ExtractRegisterAs[SLong.type](ByIndex(Inputs, 0), reg1).get, LongConstant(15)), alicePubKey.isProven),
      AND(LE(ExtractRegisterAs[SLong.type](ByIndex(Inputs, 0), reg1).get, LongConstant(15)), bobPubKey.isProven)
    )

    val prop = AND(
      EQ(SizeOf(Inputs), IntConstant(3)),
      EQ(ExtractScriptBytes(ByIndex(Inputs, 0)), ByteArrayConstant(ErgoTree.fromSigmaBoolean(oraclePubKey).bytes)),
      contractLogic
    ).toSigmaProp

    val sOracle = oracleBox
    val sAlice = ErgoBox(10, prop, 0, Seq(), Map())
    val sBob = ErgoBox(10, prop, 0, Seq(), Map())

    val newBox1 = ErgoBox(20, alicePubKey, 0)
    val newBoxes = IndexedSeq(newBox1)
    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(sOracle, sAlice, sBob),
      spendingTransaction,
      self = null)

    val prA = alice.prove(emptyEnv + (ScriptNameProp -> "alice_prove"), prop, ctx, fakeMessage).get
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, prA, fakeMessage).get._1 shouldBe true
  }

  case class OracleContract[Spec <: ContractSpec]
      ( temperature: Long,
      oracle: Spec#ProvingParty, alice: Spec#ProvingParty, bob: Spec#ProvingParty)
      (implicit val spec: Spec) extends SigmaContractSyntax with StdContracts
  {
    import syntax._
    def pkOracle = oracle.pubKey
    def pkA = alice.pubKey
    def pkB = bob.pubKey
    def inRegId = reg1.asIndex

    lazy val contractEnv = Env("pkA" -> pkA, "pkB" -> pkB, "pkOracle" -> pkOracle, "inRegId" -> inRegId)

    lazy val prop = proposition("buyer", { ctx: Context =>
      import ctx._
      val okInputs = INPUTS.length == 3
      val okInput0 = INPUTS(0).propositionBytes == pkOracle.propBytes
      val inReg = INPUTS(0).R4[Long].get
      val okContractLogic = (inReg > 15L && pkA) || (inReg <= 15L && pkB)
      okInputs && okInput0 && okContractLogic
    },
    """{
     |      val okInputs = INPUTS.size == 3
     |      val okInput0 = INPUTS(0).propositionBytes == pkOracle.propBytes
     |      val inReg = INPUTS(0).R4[Long].get
     |      val okContractLogic = (inReg > 15L && pkA) || (inReg <= 15L && pkB)
     |      okInputs && okInput0 && okContractLogic
     |}
    """.stripMargin)

    lazy val oracleSignature = proposition("oracleSignature", _ => pkOracle, "pkOracle")
    lazy val aliceSignature  = proposition("aliceSignature", _ => pkA, "pkA")
  }

  lazy val spec = TestContractSpec(suite)(new TestingIRContext)
  lazy val oracle = spec.ProvingParty("Alice")
  lazy val alice = spec.ProvingParty("Alice")
  lazy val bob = spec.ProvingParty("Bob")

  property("lightweight oracle example (ErgoDsl)") {
    val temperature: Long = 18
    val contract = OracleContract[spec.type](temperature, oracle, alice, bob)(spec)
    import contract.spec._

    // ARRANGE
    // block, tx, and output boxes which we will spend
    val mockTx = block(0).newTransaction()
    val sOracle = mockTx
        .outBox(value = 1L, contract.oracleSignature)
        .withRegs(reg1 -> temperature)

    val sAlice = mockTx.outBox(10, contract.prop)
    val sBob   = mockTx.outBox(10, contract.prop)

    val tx = block(50).newTransaction().spending(sOracle, sAlice, sBob)
    tx.outBox(20, contract.aliceSignature)
    val in = tx.inputs(1)
    val res = in.runDsl()
    res shouldBe alice.pubKey

    val pr = alice.prove(in).get
    contract.verifier.verify(in, pr) shouldBe true
  }
}
