package sigmastate.utxo.examples

import scorex.crypto.hash.Blake2b256
import sigmastate.Values.ByteArrayConstant
import sigmastate.eval.{CompiletimeCosting, IRContext}
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.interpreter.Interpreter.ScriptNameProp
import sigmastate.serialization.ErgoTreeSerializer
import sigmastate.lang.Terms._

import scala.util.Random

class LetsSpecification extends SigmaTestingCommons { suite =>
  // Not mixed with TestContext since it is not possible to call compiler.compile outside tests if mixed
  implicit lazy val IR: IRContext = new IRContext with CompiletimeCosting

  val env = Map(
    ScriptNameProp -> "withdrawalScriptEnv",
    "letsToken" -> ByteArrayConstant(Array.fill(32)(Random.nextInt(100).toByte))
  )

  val exchangeScript = compiler.compile(env,
    """{
      |
      |  val minBalance = -20000
      |
      |  val lookupProof = getVar[Coll[Byte]](2).get
      |
      |  val treeHolderBox = CONTEXT.dataInputs(0)
      |
      |  val properTree = treeHolderBox.tokens(0)._1 == letsToken
      |  val membersTree = treeHolderBox.R4[AvlTree].get
      |
      |  val participant0 = INPUTS(0)
      |  val participant1 = INPUTS(1)
      |
      |  val participantOut0 = OUTPUTS(0)
      |  val participantOut1 = OUTPUTS(1)
      |
      |  val token0 = participant0.tokens(0)._1
      |  val token1 = participant1.tokens(1)._1
      |
      |  val memberTokens = Coll(token0, token1)
      |
      |  val membersExist = membersTree.getMany(memberTokens, lookupProof).forall({ (o: Option[Coll[Byte]]) => o.isDefined })
      |
      |  val initialBalance0 = participant0.R4[Long].get
      |
      |  val initialBalance1 = participant1.R4[Long].get
      |
      |  val finishBalance0  = participantOut0.R4[Long].get
      |
      |  val finishBalance1  = participantOut1.R4[Long].get
      |
      |  val script0Saved = participantOut0.propositionBytes == participant0.propositionBytes
      |  val script1Saved = participantOut1.propositionBytes == participant1.propositionBytes
      |  val scriptsSaved = script0Saved && script1Saved
      |
      |  val diff0 = finishBalance0 - initialBalance0
      |  val diff1 = finishBalance1 - initialBalance1
      |
      |  val diffCorrect = diff0 == -diff1
      |
      |  val balancesCorrect = (finishBalance0 > minBalance) && (finishBalance1 > minBalance) && diffCorrect
      |
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
      | // Checks that the management label token is replicating self
      | val outTokenCorrect = (selfOut.tokens.size == 1) && (selfOut.tokens(0)._1 == letsToken)
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
      | // Checks that new token is issued, and its amount is correct
      | // OUTPUTS(0) tokens already checked
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

}
