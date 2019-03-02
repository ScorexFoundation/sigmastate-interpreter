package sigmastate.utxo.examples

import java.security.SecureRandom

import com.google.common.primitives.Longs
import org.ergoplatform.ErgoBox.RegisterId
import org.ergoplatform._
import org.ergoplatform.dsl.ContractSyntax.{ErgoScript, Proposition, Token}
import org.ergoplatform.dsl.{ContractSpec, SigmaContractSyntax, StdContracts, TestContractSpec}
import scorex.crypto.authds.avltree.batch.BatchAVLProver
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate._
import sigmastate.eval.IRContext
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigmastate.utxo._
import special.collection.Coll
import special.sigma.Context


class OracleTokenExamplesSpecification extends SigmaTestingCommons { suite =>
  implicit lazy val IR = new TestingIRContext

  private val reg1 = ErgoBox.nonMandatoryRegisters(0)
  private val reg2 = ErgoBox.nonMandatoryRegisters(1)
  private val reg3 = ErgoBox.nonMandatoryRegisters(2)
  private val reg4 = ErgoBox.nonMandatoryRegisters(3)

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
  private lazy val tokenId: Coll[Byte] = spec.Coll(Blake2b256("token1"))
  property("oracle token example") {
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
        tokenID: Coll[Byte],
        alice: Spec#ProvingParty, bob: Spec#ProvingParty)
      (implicit val spec: Spec) extends SigmaContractSyntax with StdContracts
  {
    import syntax._
    def pkA = alice.pubKey
    def pkB = bob.pubKey
    def inRegId = reg1.asIndex


    lazy val env = Env("pkA" -> pkA, "pkB" -> pkB, "inRegId" -> inRegId, "tokenId" -> tokenId)

    lazy val prop = proposition("buyer", { ctx: Context =>
      import ctx._
      val okInputs = INPUTS.length == 3
      val inReg = INPUTS(0).R4[Long].get
      val inToken = INPUTS(0).tokens(0)._1 == tokenId
      val okContractLogic = (inReg > 15L && pkA) || (inReg <= 15L && pkB)
      okInputs && inToken && okContractLogic
    },
    env,
    """{
     |      val okInputs = INPUTS.size == 3
     |      val inReg = INPUTS(0).R4[Long].get
     |      val inToken = INPUTS(0).tokens(0)._1 == tokenId
     |      val okContractLogic = (inReg > 15L && pkA) || (inReg <= 15L && pkB)
     |      okInputs && inToken && okContractLogic
     |}
    """.stripMargin)

    lazy val aliceSignature  = proposition("aliceSignature", _ => pkA, env, "pkA")

    lazy val dummySignature  = proposition("dummySignature", _ => pkA, env, "pkA")
  }

  lazy val spec = TestContractSpec(suite)(new TestingIRContext)
  //lazy val oracle = spec.ProvingParty("Alice")
  lazy val alice = spec.ProvingParty("Alice")
  lazy val bob = spec.ProvingParty("Bob")

  property("lightweight oracle example (ErgoDsl)") {
    val temperature: Long = 18
    val contract = OracleContract[spec.type](temperature, tokenId, alice, bob)(spec)
    import contract.spec._

    // ARRANGE
    // block, tx, and output boxes which we will spend
    val mockTx = block(0).newTransaction()
    val sOracle = mockTx
        .outBox(value = 1L, contract.dummySignature)
        .withRegs(reg1 -> temperature)
        .withTokens(Token(tokenId, 1))

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
