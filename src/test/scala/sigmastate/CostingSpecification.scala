package sigmastate

import org.ergoplatform.ErgoLikeContext.{dummyPubkey, dummyPreHeader, noHeaders, noBoxes}
import org.ergoplatform.{ErgoLikeContext, ErgoBox}
import org.ergoplatform.ErgoScriptPredef.TrueProp
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.crypto.authds.avltree.batch.Lookup
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{ByteArrayConstant, AvlTreeConstant, BooleanConstant, IntConstant}
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.ContextExtension
import sigmastate.interpreter.Interpreter.{ScriptNameProp, ScriptEnv}
import sigmastate.utxo.CostTable
import sigmastate.utxo.CostTable._
import sigmastate.eval._
import sigmastate.eval.Extensions._
import special.sigma.{SigmaTestingData, AvlTree}

class CostingSpecification extends SigmaTestingData {
  implicit lazy val IR = new TestingIRContext {
    override val okPrintEvaluatedEntries = false
    substFromCostTable = false
  }
  lazy val interpreter = new ContextEnrichingTestProvingInterpreter
  lazy val pkA = interpreter.dlogSecrets(0).publicImage
  lazy val pkB = interpreter.dlogSecrets(1).publicImage

  val printCosts = true

  val (key1, _, avlProver) = sampleAvlProver
  val keys = Colls.fromItems(key1)
  avlProver.performOneOperation(Lookup(ADKey @@ key1.toArray))
  val digest = avlProver.digest.toColl
  val lookupProof = avlProver.generateProof().toColl
  val avlTreeData = AvlTreeData(ADDigest @@ digest.toArray, AvlTreeFlags.AllOperationsAllowed, 32, None)
  val avlTree: AvlTree = CAvlTree(avlTreeData)

  val env: ScriptEnv = Map(
    ScriptNameProp -> s"filename_verify",
    "key1" -> key1,
    "keys" -> keys,
    "lookupProof" -> lookupProof
  )

  val extension: ContextExtension = ContextExtension(Map(
    1.toByte -> IntConstant(1),
    2.toByte -> BooleanConstant(true)
  ))
  val tokenId = Blake2b256("tokenA")
  val selfBox = createBox(0, TrueProp, Seq(tokenId -> 10L),
      Map(ErgoBox.R4 -> ByteArrayConstant(Array[Byte](1, 2, 3)),
          ErgoBox.R5 -> IntConstant(3),
          ErgoBox.R6 -> AvlTreeConstant(avlTree)))
  lazy val outBoxA = ErgoBox(10, pkA, 0)
  lazy val outBoxB = ErgoBox(20, pkB, 0)
  lazy val tx = createTransaction(IndexedSeq(outBoxA, outBoxB))
  lazy val context =
    new ErgoLikeContext(
      currentHeight = preHeader.height,
      lastBlockUtxoRoot = header2.stateRoot.asInstanceOf[CAvlTree].treeData,
      minerPubkey = preHeader.minerPk.getEncoded.toArray,
      headers = headers, preHeader = preHeader,
      dataBoxes = IndexedSeq(dataBox),
      boxesToSpend = IndexedSeq(selfBox),
      spendingTransaction = tx, self = selfBox, extension)

  def cost(script: String): Long = {
    val ergoTree = compiler.compile(env, script)
    val res = interpreter.reduceToCrypto(context, env, ergoTree).get._2
    if (printCosts)
      println(script + s" --> cost $res")
    res
  }

  val ContextVarAccess = accessContextVar + selectField  // `getVar(id)` + `.get`
  val RegisterAccess = accessRegister + selectField  // `getReg(id)` + `.get`
  val GTConstCost = comparisonCost + constCost
  val LengthGTConstCost = collLength + GTConstCost
  val LengthGTCost = collLength + comparisonCost  // can be used when constCost is already accumulated

  property("basic (smoke) tests") {

    cost("{ getVar[Boolean](2).get }") shouldBe ContextVarAccess

    cost("{ getVar[Int](1).get > 1 }") shouldBe (ContextVarAccess + GTConstCost)

    // accessing two context variables
    cost("{ getVar[Int](1).get > 1 && getVar[Boolean](2).get }") shouldBe
        (ContextVarAccess * 2 + GTConstCost + logicCost)

    // the same var is used twice doesn't lead to double cost
    cost("{ getVar[Int](1).get + 1 > getVar[Int](1).get }") shouldBe
        (ContextVarAccess + plusMinus + constCost + comparisonCost)

    // cost is accumulated along the expression tree
    cost("{ getVar[Int](1).get + 1 > getVar[Int](1).get && getVar[Boolean](2).get }") shouldBe
        (ContextVarAccess * 2 + plusMinus + constCost + comparisonCost + logicCost)
  }

  property("logical op costs") {
    cost("{ val cond = getVar[Boolean](2).get; cond && cond }") shouldBe (ContextVarAccess + logicCost)
    cost("{ val cond = getVar[Boolean](2).get; cond || cond }") shouldBe (ContextVarAccess + logicCost)
    cost("{ val cond = getVar[Boolean](2).get; cond || cond && true }") shouldBe (ContextVarAccess + logicCost * 2 + constCost)
    cost("{ val cond = getVar[Boolean](2).get; cond || cond && true || cond }") shouldBe (ContextVarAccess + logicCost * 3 + constCost)
    cost("{ val cond = getVar[Boolean](2).get; cond ^ cond && true ^ cond }") shouldBe (ContextVarAccess + logicCost * 3 + constCost)
    cost("{ val cond = getVar[Boolean](2).get; allOf(Coll(cond, true, cond)) }") shouldBe (ContextVarAccess + logicCost * 2 + constCost)
  }

  property("SELF box operations cost") {
    cost("{ SELF.value > 0 }") shouldBe (accessBox + extractCost + GTConstCost)
    cost("{ SELF.id.size > 0 }") shouldBe (accessBox + extractCost + LengthGTConstCost)
    cost("{ SELF.tokens.size > 0 }") shouldBe (accessBox + extractCost + LengthGTConstCost)
    cost("{ SELF.creationInfo._1 > 0 }") shouldBe (accessBox + accessRegister + selectField + GTConstCost)
    cost("{ SELF.R5[Int].get > 0 }") shouldBe (accessBox + RegisterAccess + GTConstCost)

// TODO   cost("{ SELF.getReg[Long](0.toByte).get > 0 }") shouldBe (accessBox + RegisterAccess + GTConstCost)
  }

  lazy val OutputsCost = selectField + accessBox * tx.outputs.length
  lazy val InputsCost = selectField + accessBox * context.boxesToSpend.length
  lazy val DataInputsCost = selectField + accessBox * context.dataBoxes.length
  lazy val HeadersCost = selectField
  lazy val PreHeaderCost = selectField
  lazy val AccessHeaderCost = selectField + collByIndex + constCost

  property("Global operations cost") {
    // TODO cost("{ groupGenerator.isIdentity > 0 }") shouldBe (selectField + selectField + GTConstCost)
  }

  property("Context operations cost") {
    cost("{ HEIGHT > 0 }") shouldBe (selectField + GTConstCost)
    cost("{ OUTPUTS.size > 0 }") shouldBe (OutputsCost + LengthGTConstCost)
    cost("{ INPUTS.size > 0 }") shouldBe (InputsCost + LengthGTConstCost)
    cost("{ CONTEXT.dataInputs.size > 0 }") shouldBe (DataInputsCost + LengthGTConstCost)
    cost("{ LastBlockUtxoRootHash.isUpdateAllowed }") shouldBe (selectField + selectField)
    cost("{ MinerPubkey.size > 0 }") shouldBe (selectField + LengthGTConstCost)
    cost("{ CONTEXT.headers.size > 0 }") shouldBe (HeadersCost + LengthGTConstCost)
    cost("{ CONTEXT.preHeader.height > 0 }") shouldBe (PreHeaderCost + selectField + GTConstCost)
  }

  property("PreHeader operations cost") {
    cost("{ CONTEXT.preHeader.version > 0 }") shouldBe (PreHeaderCost + selectField + castOp + GTConstCost)
    cost("{ CONTEXT.preHeader.parentId.size > 0 }") shouldBe (PreHeaderCost + selectField + LengthGTConstCost)
    cost("{ CONTEXT.preHeader.timestamp > 0L }") shouldBe (PreHeaderCost + selectField + GTConstCost)
    cost("{ CONTEXT.preHeader.nBits > 0L }") shouldBe (PreHeaderCost + selectField + GTConstCost)
    cost("{ CONTEXT.preHeader.height > 0 }") shouldBe (PreHeaderCost + selectField + GTConstCost)

    cost("{ CONTEXT.preHeader.minerPk == groupGenerator }") shouldBe
      (PreHeaderCost + selectField + comparisonCost + selectField)

    cost("{ CONTEXT.preHeader.votes.size > 0 }") shouldBe
      (PreHeaderCost + selectField + LengthGTConstCost)
  }

  property("Header operations cost") {
    val header = "CONTEXT.headers(0)"
    cost(s"{ $header.id.size > 0 }") shouldBe (AccessHeaderCost + selectField + LengthGTCost)
    cost(s"{ $header.version > 0 }") shouldBe (AccessHeaderCost + selectField + castOp + comparisonCost)
    cost(s"{ $header.parentId.size > 0 }") shouldBe (AccessHeaderCost + selectField + LengthGTCost)
    cost(s"{ $header.ADProofsRoot.size > 0 }") shouldBe (AccessHeaderCost + selectField + LengthGTCost)
    cost(s"{ $header.stateRoot.isUpdateAllowed }") shouldBe (AccessHeaderCost + selectField + selectField)
    cost(s"{ $header.transactionsRoot.size > 0 }") shouldBe (AccessHeaderCost + selectField + LengthGTCost)
    cost(s"{ $header.timestamp > 0L }") shouldBe (AccessHeaderCost + selectField + GTConstCost)
    cost(s"{ $header.nBits > 0L }") shouldBe (AccessHeaderCost + selectField + GTConstCost)
    cost(s"{ $header.height > 0 }") shouldBe (AccessHeaderCost + selectField + comparisonCost)
    cost(s"{ $header.extensionRoot.size > 0 }") shouldBe (AccessHeaderCost + selectField + LengthGTCost)

    cost(s"{ $header.minerPk == groupGenerator }") shouldBe
        (AccessHeaderCost + selectField + comparisonCost + selectField)

    cost(s"{ $header.powOnetimePk == groupGenerator }") shouldBe
        (AccessHeaderCost + selectField + comparisonCost + selectField)

    cost(s"{ $header.powNonce.size > 0 }") shouldBe (AccessHeaderCost + selectField + LengthGTCost)

    cost(s"{ $header.powDistance > 0 }") shouldBe (AccessHeaderCost + selectField + comparisonBigInt + constCost)
    cost(s"{ $header.votes.size > 0 }") shouldBe (AccessHeaderCost + selectField + LengthGTCost)
  }

  val AccessRootHash = selectField
  def perKbCostOf(dataSize: Long, opCost: Int) = {
    ((dataSize / 1024L).toInt + 1) * opCost
  }
  import Sized._

  property("AvlTree operations cost") {
    val rootTree = "LastBlockUtxoRootHash"
    cost(s"{ $rootTree.digest.size > 0 }") shouldBe (AccessRootHash + selectField + LengthGTConstCost)
    cost(s"{ $rootTree.enabledOperations > 0 }") shouldBe (AccessRootHash + selectField + castOp + GTConstCost)
    cost(s"{ $rootTree.keyLength > 0 }") shouldBe (AccessRootHash + selectField + GTConstCost)
    cost(s"{ $rootTree.isInsertAllowed }") shouldBe (AccessRootHash + selectField)
    cost(s"{ $rootTree.isUpdateAllowed }") shouldBe (AccessRootHash + selectField)
    cost(s"{ $rootTree.isRemoveAllowed }") shouldBe (AccessRootHash + selectField)
    cost(s"{ $rootTree.updateDigest($rootTree.digest) == $rootTree }") shouldBe (AccessRootHash + selectField + newAvlTreeCost + comparisonPerKbCost)
    cost(s"{ $rootTree.updateOperations(1.toByte) == $rootTree }") shouldBe (AccessRootHash + newAvlTreeCost + comparisonPerKbCost + constCost)

    val AccessTree = accessBox + RegisterAccess
    val selfTree = "SELF.R6[AvlTree].get"
    val sizeOfArgs = Seq(sizeOf(avlTree), sizeOf(key1), sizeOf(lookupProof)).foldLeft(0L)(_ + _.dataSize)
    val containsCost = perKbCostOf(sizeOfArgs, avlTreeOp)

    cost(s"{ $selfTree.contains(key1, lookupProof) }") shouldBe (AccessTree + containsCost + constCost)
    cost(s"{ $selfTree.get(key1, lookupProof).isDefined }") shouldBe (AccessTree + containsCost + constCost + selectField)
    cost(s"{ $selfTree.getMany(keys, lookupProof).size > 0 }") shouldBe (AccessTree + containsCost + constCost + LengthGTConstCost)
  }

  property("Coll operations cost") {
    val coll = "OUTPUTS"
    cost(s"{ $coll.filter({ (b: Box) => b.value > 1L }).size > 0 }") shouldBe
      (lambdaCost + accessBox + extractCost + GTConstCost + selectField +
        (accessBox + comparisonCost) * tx.outputs.length + collToColl + LengthGTConstCost)
    cost(s"{ $coll.flatMap({ (b: Box) => b.propositionBytes }).size > 0 }") shouldBe
      (lambdaCost + accessBox + extractCost + selectField +
        accessBox  * tx.outputs.length + collToColl + LengthGTConstCost)
  }

  property("Option operations cost") {
    val opt = "SELF.R5[Int]"
    val accessOpt = accessBox + accessRegister
    cost(s"{ $opt.get > 0 }") shouldBe (accessOpt + selectField + GTConstCost)
    cost(s"{ $opt.isDefined }") shouldBe (accessOpt + selectField)
    cost(s"{ $opt.getOrElse(1) > 0 }") shouldBe (accessOpt + selectField + GTConstCost)
    cost(s"{ $opt.filter({ (x: Int) => x > 0}).isDefined }") shouldBe
       (accessOpt + OptionOp + lambdaCost + GTConstCost + selectField)
    cost(s"{ $opt.map({ (x: Int) => x + 1}).isDefined }") shouldBe
      (accessOpt + OptionOp + lambdaCost + plusMinus + constCost + selectField)
  }
}