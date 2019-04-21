package sigmastate.utxo.examples

import org.ergoplatform._
import org.ergoplatform.ErgoBox.{R4, R5}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Lookup}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.{AvlTreeData, AvlTreeFlags, TrivialProp}
import sigmastate.Values.{AvlTreeConstant, ByteArrayConstant, LongConstant, SigmaPropConstant}
import sigmastate.eval.{CompiletimeCosting, IRContext}
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter.ScriptNameProp
import sigmastate.serialization.ErgoTreeSerializer
import sigmastate.lang.Terms._

import scala.util.Random

class LetsSpecification extends SigmaTestingCommons {
  suite =>
  // Not mixed with TestContext since it is not possible to call compiler.compile outside tests if mixed
  implicit lazy val IR: IRContext = new IRContext with CompiletimeCosting

  lazy val project = new ErgoLikeTestProvingInterpreter()

  val letsTokenId = Digest32 @@ Array.fill(32)(Random.nextInt(100).toByte)

  val env = Map(ScriptNameProp -> "withdrawalScriptEnv", "letsToken" -> ByteArrayConstant(letsTokenId))

  private val miningRewardsDelay = 720
  private val feeProp = ErgoScriptPredef.feeProposition(miningRewardsDelay)

  val exchangeScript = compiler.compile(env,
    """{
      |
      |  // Minimal balance allowed for LETS trader
      |  val minBalance = -20000
      |
      |  val lookupProof = getVar[Coll[Byte]](1).get
      |
      |  // The read-only box which contains directory of LETS members
      |  val treeHolderBox = CONTEXT.dataInputs(0)
      |  val properTree = treeHolderBox.tokens(0)._1 == letsToken
      |  val membersTree = treeHolderBox.R4[AvlTree].get
      |
      |  // A spending transaction is taking two boxes of LETS members willing to make a deal,
      |  // and returns boxes with modified balances.
      |  val participant0 = INPUTS(0)
      |  val participant1 = INPUTS(1)
      |  val participantOut0 = OUTPUTS(0)
      |  val participantOut1 = OUTPUTS(1)
      |
      |  //Check that members are indeed belong to the LETS
      |  val token0 = participant0.tokens(0)._1
      |  val token1 = participant1.tokens(0)._1
      |  val memberTokens = Coll(token0, token1)
      |  val membersExist = membersTree.getMany(memberTokens, lookupProof).forall({ (o: Option[Coll[Byte]]) => o.isDefined })
      |
      |  // Check that LETS member balance changes during the deal are correct
      |  val initialBalance0 = participant0.R4[Long].get
      |  val initialBalance1 = participant1.R4[Long].get
      |  val finishBalance0  = participantOut0.R4[Long].get
      |  val finishBalance1  = participantOut1.R4[Long].get
      |  val diff0 = finishBalance0 - initialBalance0
      |  val diff1 = finishBalance1 - initialBalance1
      |  val diffCorrect = diff0 == -diff1
      |  val balancesCorrect = (finishBalance0 > minBalance) && (finishBalance1 > minBalance) && diffCorrect
      |
      |  // Check that member boxes save their scripts.
      |  // todo: optimization could be made here
      |  val script0Saved = participantOut0.propositionBytes == participant0.propositionBytes
      |  val script1Saved = participantOut1.propositionBytes == participant1.propositionBytes
      |  val scriptsSaved = script0Saved && script1Saved
      |
      |  // Member-specific box protection
      |  val selfPubKey = SELF.R5[SigmaProp].get
      |
      |  selfPubKey && properTree && membersExist && diffCorrect && scriptsSaved
      |}""".stripMargin
  ).asSigmaProp

  val userContractHash = Blake2b256(ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(exchangeScript))

  val managementScript = compiler.compile(env.updated("userContractHash", userContractHash),
    """{
      |
      | val selfOut = OUTPUTS(0)
      |
      | // Management script
      | val managementScript = selfOut.R5[SigmaProp].get
      |
      | // The management script template is replicating self, and management script is satisfied
      | val scriptCorrect = (selfOut.propositionBytes == SELF.propositionBytes) && managementScript
      |
      | // A spending transaction is creating boxes for directory, user, fee.
      | val outsSizeCorrect = OUTPUTS.size == 3
      |
      | // Checks that the management label token is replicating self
      | val outTokenCorrect = (selfOut.tokens.size == 1) && (selfOut.tokens(0)._1 == letsToken)
      |
      | // Checks that new token is issued, and its amount is correct
      | // OUTPUTS(0) tokens already checked via outtokenCorrect
      | val issuedTokenId = INPUTS(0).id
      | val userOut = OUTPUTS(1)
      | val correctTokenAmounts =
      |   (userOut.tokens.size == 1 &&
      |     userOut.tokens(0)._1 == issuedTokenId &&
      |     userOut.tokens(0)._2 == 1 &&
      |     OUTPUTS(2).tokens.size == 0 &&
      |     outTokenCorrect)
      |
      |  // Checks that the new user has been created with the zero balance
      |  val zeroUserBalance  = userOut.R4[Long].get == 0
      |
      |  val properUserScript = blake2b256(userOut.propositionBytes) == userContractHash
      |
      |  // Checks that the new token identifier has been added to the directory
      |  val selfTree = SELF.R4[AvlTree].get
      |  val toAdd: Coll[(Coll[Byte], Coll[Byte])] = Coll((issuedTokenId, Coll[Byte]()))
      |  val proof = getVar[Coll[Byte]](1).get
      |  val modifiedTree = selfTree.insert(toAdd, proof).get
      |  val expectedTree = selfOut.R4[AvlTree].get
      |  val treeCorrect = modifiedTree == expectedTree
      |
      |  correctTokenAmounts && scriptCorrect && treeCorrect && zeroUserBalance && properUserScript
      |}""".stripMargin
  ).asSigmaProp

  println(exchangeScript)
  println(managementScript)

  property("adding new member") {
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    val digest = avlProver.digest
    val initTreeData = new AvlTreeData(digest, AvlTreeFlags.InsertOnly, 32, None)

    val projectBoxBefore = ErgoBox(10, managementScript, 0,
      Seq(letsTokenId -> 1L),
      Map(R4 -> AvlTreeConstant(initTreeData), R5 -> SigmaPropConstant(TrivialProp.TrueProp)))

    val userTokenId = Digest32 @@ projectBoxBefore.id

    avlProver.performOneOperation(Insert(ADKey @@ userTokenId, ADValue @@ Array.emptyByteArray))

    val proof = avlProver.generateProof()
    val endTree = new AvlTreeData(avlProver.digest, AvlTreeFlags.InsertOnly, 32, None)

    val projectBoxAfter = ErgoBox(9, managementScript, 0,
      Seq(letsTokenId -> 1L),
      Map(R4 -> AvlTreeConstant(endTree), R5 -> SigmaPropConstant(TrivialProp.TrueProp)))
    val feeBox = ErgoBox(1, feeProp, 0, Seq(), Map())
    val userBox = ErgoBox(1, exchangeScript, 0, Seq(userTokenId -> 1L), Map(R4 -> LongConstant(0)))

    val issuanceTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(projectBoxAfter, userBox, feeBox))

    val fundingContext = ErgoLikeContext(
      currentHeight = 1000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(projectBoxBefore),
      spendingTransaction = issuanceTx,
      self = projectBoxBefore)

    val managementProver = new ContextEnrichingTestProvingInterpreter()
      .withContextExtender(1, ByteArrayConstant(proof))

    val res = managementProver.prove(env, managementScript, fundingContext, fakeMessage).get
    println("new user script cost: " + res.cost)
  }

  property("exchange") {

    val userTokenId0 = Digest32 @@ Array.fill(32)(Random.nextInt(100).toByte)
    val userTokenId1 = Digest32 @@ Array.fill(32)(Random.nextInt(100).toByte)

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    avlProver.performOneOperation(Insert(ADKey @@ userTokenId0, ADValue @@ Array.emptyByteArray))
    avlProver.performOneOperation(Insert(ADKey @@ userTokenId1, ADValue @@ Array.emptyByteArray))
    val digest = avlProver.digest
    avlProver.generateProof()
    val initTreeData = new AvlTreeData(digest, AvlTreeFlags.InsertOnly, 32, None)

    avlProver.performOneOperation(Lookup(ADKey @@ userTokenId0))
    avlProver.performOneOperation(Lookup(ADKey @@ userTokenId1))
    val proof = avlProver.generateProof()

    val directoryBox = ErgoBox(10, managementScript, 0,
      Seq(letsTokenId -> 1L),
      Map(R4 -> AvlTreeConstant(initTreeData), R5 -> SigmaPropConstant(TrivialProp.TrueProp)))

    val directoryDataInput = DataInput(directoryBox.id)

    val userBoxBefore0 = ErgoBox(1, exchangeScript, 0, Seq(userTokenId0 -> 1L),
      Map(R4 -> LongConstant(0), R5 -> SigmaPropConstant(TrivialProp.TrueProp)))
    val userBoxBefore1 = ErgoBox(1, exchangeScript, 0, Seq(userTokenId1 -> 1L),
      Map(R4 -> LongConstant(0), R5 -> SigmaPropConstant(TrivialProp.TrueProp)))

    val userBoxAfter0 = ErgoBox(1, exchangeScript, 0, Seq(userTokenId0 -> 1L), Map(R4 -> LongConstant(-5)))
    val userBoxAfter1 = ErgoBox(1, exchangeScript, 0, Seq(userTokenId1 -> 1L), Map(R4 -> LongConstant(5)))

    val issuanceTx = new ErgoLikeTransaction(IndexedSeq(), IndexedSeq(directoryDataInput), IndexedSeq(userBoxAfter0, userBoxAfter1))

    val exchangeContext = ErgoLikeContext(
      currentHeight = 1000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      dataBoxes = IndexedSeq(directoryBox),
      boxesToSpend = IndexedSeq(userBoxBefore0, userBoxBefore1),
      spendingTransaction = issuanceTx,
      self = userBoxBefore0)

    val managementProver = new ContextEnrichingTestProvingInterpreter()
      .withContextExtender(1, ByteArrayConstant(proof))

    val res = managementProver.prove(env, exchangeScript, exchangeContext, fakeMessage).get
    println("exchange script cost: " + res.cost)
  }

}
