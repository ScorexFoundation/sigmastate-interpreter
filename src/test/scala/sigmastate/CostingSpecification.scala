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
import Sized._
import org.ergoplatform.ErgoConstants.ScriptCostLimit
import org.ergoplatform.validation.ValidationRules

class CostingSpecification extends SigmaTestingData {
  implicit lazy val IR = new TestingIRContext {
//    override val okPrintEvaluatedEntries = true
    substFromCostTable = false
  }
  lazy val interpreter = new ContextEnrichingTestProvingInterpreter
  lazy val pkA = interpreter.dlogSecrets(0).publicImage
  lazy val pkB = interpreter.dlogSecrets(1).publicImage

  val printCosts = true

  val (key1, _, avlProver) = sampleAvlProver
  val keys = Colls.fromItems(key1)
  val key2 = keyCollGen.sample.get
  avlProver.performOneOperation(Lookup(ADKey @@ key1.toArray))
  val digest = avlProver.digest.toColl
  val lookupProof = avlProver.generateProof().toColl
  val avlTreeData = AvlTreeData(ADDigest @@ digest.toArray, AvlTreeFlags.AllOperationsAllowed, 32, None)
  val avlTree: AvlTree = CAvlTree(avlTreeData)

  lazy val env: ScriptEnv = Map(
    ScriptNameProp -> s"filename_verify",
    "key1" -> key1,
    "key2" -> key2,
    "keys" -> keys,
    "lookupProof" -> lookupProof,
    "pkA" -> pkA,
    "pkB" -> pkB,
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
      spendingTransaction = tx, self = selfBox, extension, ValidationRules.currentSettings, ScriptCostLimit.value, CostTable.interpreterInitCost)

  def cost(script: String)(expCost: Int): Unit = {
    val ergoTree = compiler.compile(env, script)
    val res = interpreter.reduceToCrypto(context, env, ergoTree).get._2
    if (printCosts)
      println(script + s" --> cost $res")
    res shouldBe ((expCost * CostTable.costFactorIncrease / CostTable.costFactorDecrease) + CostTable.interpreterInitCost).toLong
  }

  val ContextVarAccess = getVarCost + selectField  // `getVar(id)` + `.get`
  val RegisterAccess = accessRegister + selectField  // `getReg(id)` + `.get`
  val GTConstCost = comparisonCost + constCost
  val LengthGTConstCost = collLength + GTConstCost
  val LengthGTCost = collLength + comparisonCost  // can be used when constCost is already accumulated

  property("basic (smoke) tests") {

    cost("{ getVar[Boolean](2).get }")(ContextVarAccess)

    cost("{ getVar[Int](1).get > 1 }")(ContextVarAccess + GTConstCost)

    // accessing two context variables
    cost("{ getVar[Int](1).get > 1 && getVar[Boolean](2).get }")(ContextVarAccess * 2 + GTConstCost + logicCost)

    // the same var is used twice doesn't lead to double cost
    cost("{ getVar[Int](1).get + 1 > getVar[Int](1).get }")(ContextVarAccess + plusMinus + constCost + comparisonCost)

    // cost is accumulated along the expression tree
    cost("{ getVar[Int](1).get + 1 > getVar[Int](1).get && getVar[Boolean](2).get }")(
      ContextVarAccess * 2 + plusMinus + constCost + comparisonCost + logicCost)
  }

  property("logical op costs") {
    cost("{ val cond = getVar[Boolean](2).get; cond && cond }")(ContextVarAccess + logicCost)
    cost("{ val cond = getVar[Boolean](2).get; cond || cond }")(ContextVarAccess + logicCost)
    cost("{ val cond = getVar[Boolean](2).get; cond || cond && true }")(ContextVarAccess + logicCost * 2 + constCost)
    cost("{ val cond = getVar[Boolean](2).get; cond || cond && true || cond }")(ContextVarAccess + logicCost * 3 + constCost)
    cost("{ val cond = getVar[Boolean](2).get; cond ^ cond && true ^ cond }")(ContextVarAccess + logicCost * 3 + constCost)
    cost("{ val cond = getVar[Boolean](2).get; allOf(Coll(cond, true, cond)) }")(ContextVarAccess + logicCost * 2 + constCost)
    cost("{ val cond = getVar[Boolean](2).get; anyOf(Coll(cond, true, cond)) }") shouldBe (ContextVarAccess + logicCost * 2 + constCost)
    cost("{ val cond = getVar[Boolean](2).get; xorOf(Coll(cond, true, cond)) }") shouldBe (ContextVarAccess + logicCost * 2 + constCost)
  }

  property("atLeast costs") {
    val concrCollCost = proveDlogEvalCost
    cost("{ atLeast(2, Coll(pkA, pkB, pkB)) }") shouldBe
      (concrCollCost + proveDlogEvalCost * 3 + logicCost + constCost + concreteCollCost)
  }

  property("allZK costs") {
    val concrCollCost = proveDlogEvalCost
    cost("{ pkA && pkB }") shouldBe (concrCollCost + sigmaAndCost * 2)
  }

  property("anyZK costs") {
    val concrCollCost = proveDlogEvalCost
    cost("{ pkA || pkB }") shouldBe (concrCollCost + sigmaOrCost * 2)
  }

  property("blake2b256 costs") {
    cost("{ blake2b256(key1).size > 0 }") shouldBe (constCost + hashPerKb + LengthGTConstCost)
  }

  property("sha256 costs") {
    cost("{ sha256(key1).size > 0 }") shouldBe (constCost + hashPerKb + LengthGTConstCost)
  }

  property("byteArrayToBigInt") {
    cost("{ byteArrayToBigInt(Coll[Byte](1.toByte)) > 0 }") shouldBe
      (constCost // byte const
        + concreteCollCost // concrete collection
        + constCost * 1 // build from array cost
        + castOp + newBigIntPerItem + comparisonBigInt + constCost)
  }

  property("byteArrayToLong") {
    cost("{ byteArrayToLong(Coll[Byte](1.toByte, 1.toByte, 1.toByte, 1.toByte, 1.toByte, 1.toByte, 1.toByte, 1.toByte)) > 0 }") shouldBe
      (constCost // byte const
        + concreteCollCost // concrete collection
        + constCost * 8 // build from array cost
        + castOp + GTConstCost)
  }

  property("longToByteArray") {
    cost("{ longToByteArray(1L).size > 0 }") shouldBe (constCost + castOp + LengthGTConstCost)
  }

  property("decodePoint and GroupElement.getEncoded") {
    cost("{ decodePoint(groupGenerator.getEncoded) == groupGenerator }") shouldBe
      (selectField + selectField + decodePointCost + comparisonCost)
  }

  property("GroupElement.negate") {
    cost("{ groupGenerator.negate != groupGenerator }") shouldBe
      (selectField + negateGroup + comparisonCost)
  }

  property("SELF box operations cost") {
    cost("{ SELF.value > 0 }")(accessBox + extractCost + GTConstCost)
    cost("{ SELF.id.size > 0 }")(accessBox + extractCost + LengthGTConstCost)
    cost("{ SELF.tokens.size > 0 }")(accessBox + extractCost + LengthGTConstCost)
    cost("{ SELF.creationInfo._1 > 0 }")(accessBox + accessRegister + selectField + GTConstCost)
    cost("{ SELF.R5[Int].get > 0 }")(accessBox + RegisterAccess + GTConstCost)

    // TODO coverage: related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/416
    // cost("{ SELF.getReg[Long](0.toByte).get > 0 }")(accessBox + RegisterAccess + GTConstCost)
  }

  lazy val OutputsCost = selectField + accessBox * tx.outputs.length
  lazy val InputsCost = selectField + accessBox * context.boxesToSpend.length
  lazy val DataInputsCost = selectField + accessBox * context.dataBoxes.length
  lazy val HeadersCost = selectField
  lazy val PreHeaderCost = selectField
  lazy val AccessHeaderCost = selectField + collByIndex + constCost

  property("Global operations cost") {
    // TODO costing: related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
    // cost("{ groupGenerator.isIdentity > 0 }")(selectField + selectField + GTConstCost)

    val sizeOfArgs = Seq(sizeOf(key1), sizeOf(key1)).foldLeft(0L)(_ + _.dataSize)
    val xorCost = constCost + perKbCostOf(sizeOfArgs, hashPerKb / 2)
    cost("{ xor(key1, key1).size > 0 }")(xorCost + LengthGTConstCost)
  }

  property("Context operations cost") {
    cost("{ HEIGHT > 0 }")(selectField + GTConstCost)
    cost("{ OUTPUTS.size > 0 }")(OutputsCost + LengthGTConstCost)
    cost("{ INPUTS.size > 0 }")(InputsCost + LengthGTConstCost)
    cost("{ CONTEXT.dataInputs.size > 0 }")(DataInputsCost + LengthGTConstCost)
    cost("{ LastBlockUtxoRootHash.isUpdateAllowed }")(selectField + selectField)
    cost("{ MinerPubkey.size > 0 }")(selectField + LengthGTConstCost)
    cost("{ CONTEXT.headers.size > 0 }")(HeadersCost + LengthGTConstCost)
    cost("{ CONTEXT.preHeader.height > 0 }")(PreHeaderCost + selectField + GTConstCost)
  }

  property("PreHeader operations cost") {
    cost("{ CONTEXT.preHeader.version > 0 }")(PreHeaderCost + selectField + castOp + GTConstCost)
    cost("{ CONTEXT.preHeader.parentId.size > 0 }")(PreHeaderCost + selectField + LengthGTConstCost)
    cost("{ CONTEXT.preHeader.timestamp > 0L }")(PreHeaderCost + selectField + GTConstCost)
    cost("{ CONTEXT.preHeader.nBits > 0L }")(PreHeaderCost + selectField + GTConstCost)
    cost("{ CONTEXT.preHeader.height > 0 }")(PreHeaderCost + selectField + GTConstCost)

    cost("{ CONTEXT.preHeader.minerPk == groupGenerator }")(
      PreHeaderCost + selectField + comparisonCost + selectField)

    cost("{ CONTEXT.preHeader.votes.size > 0 }")(PreHeaderCost + selectField + LengthGTConstCost)
  }

  property("Header operations cost") {
    val header = "CONTEXT.headers(0)"
    cost(s"{ $header.id.size > 0 }")(AccessHeaderCost + selectField + LengthGTCost)
    cost(s"{ $header.version > 0 }")(AccessHeaderCost + selectField + castOp + comparisonCost)
    cost(s"{ $header.parentId.size > 0 }")(AccessHeaderCost + selectField + LengthGTCost)
    cost(s"{ $header.ADProofsRoot.size > 0 }")(AccessHeaderCost + selectField + LengthGTCost)
    cost(s"{ $header.stateRoot.isUpdateAllowed }")(AccessHeaderCost + selectField + selectField)
    cost(s"{ $header.transactionsRoot.size > 0 }")(AccessHeaderCost + selectField + LengthGTCost)
    cost(s"{ $header.timestamp > 0L }")(AccessHeaderCost + selectField + GTConstCost)
    cost(s"{ $header.nBits > 0L }")(AccessHeaderCost + selectField + GTConstCost)
    cost(s"{ $header.height > 0 }")(AccessHeaderCost + selectField + comparisonCost)
    cost(s"{ $header.extensionRoot.size > 0 }")(AccessHeaderCost + selectField + LengthGTCost)

    cost(s"{ $header.minerPk == groupGenerator }")(AccessHeaderCost + selectField + comparisonCost + selectField)

    cost(s"{ $header.powOnetimePk == groupGenerator }")(AccessHeaderCost + selectField + comparisonCost + selectField)

    cost(s"{ $header.powNonce.size > 0 }")(AccessHeaderCost + selectField + LengthGTCost)

    cost(s"{ $header.powDistance > 0 }")(AccessHeaderCost + selectField + comparisonBigInt + constCost)
    cost(s"{ $header.votes.size > 0 }")(AccessHeaderCost + selectField + LengthGTCost)
  }

  val AccessRootHash = selectField
  def perKbCostOf(dataSize: Long, opCost: Int) = {
    ((dataSize / 1024L).toInt + 1) * opCost
  }

  property("AvlTree operations cost") {
    val rootTree = "LastBlockUtxoRootHash"
//    cost(s"{ $rootTree.digest.size > 0 }")(AccessRootHash + selectField + LengthGTConstCost)
//    cost(s"{ $rootTree.enabledOperations > 0 }")(AccessRootHash + selectField + castOp + GTConstCost)
//    cost(s"{ $rootTree.keyLength > 0 }")(AccessRootHash + selectField + GTConstCost)
//    cost(s"{ $rootTree.isInsertAllowed }")(AccessRootHash + selectField)
//    cost(s"{ $rootTree.isUpdateAllowed }")(AccessRootHash + selectField)
//    cost(s"{ $rootTree.isRemoveAllowed }")(AccessRootHash + selectField)
//    cost(s"{ $rootTree.updateDigest($rootTree.digest) == $rootTree }") shouldBe
//      (AccessRootHash + selectField + newAvlTreeCost + comparisonCost /* for isConstantSize AvlTree type */)
//    cost(s"{ $rootTree.updateOperations(1.toByte) == $rootTree }") shouldBe
//      (AccessRootHash + newAvlTreeCost + comparisonCost + constCost)

    val AccessTree = accessBox + RegisterAccess
    val selfTree = "SELF.R6[AvlTree].get"
    val sizeOfArgs = Seq(sizeOf(avlTree), sizeOf(key1), sizeOf(lookupProof)).foldLeft(0L)(_ + _.dataSize)
    val containsCost = perKbCostOf(sizeOfArgs, avlTreeOp)

    cost(s"{ $selfTree.contains(key1, lookupProof) }")(AccessTree + containsCost + 2 * constCost)
    cost(s"{ $selfTree.get(key1, lookupProof).isDefined }")(AccessTree + containsCost + 2 * constCost + selectField)
    cost(s"{ $selfTree.getMany(keys, lookupProof).size > 0 }")(AccessTree + containsCost + 2 * constCost + LengthGTConstCost)
    cost(s"{ $rootTree.valueLengthOpt.isDefined }") shouldBe (AccessRootHash + selectField + selectField)
    cost(s"{ $selfTree.update(Coll[(Coll[Byte], Coll[Byte])]((key1, key1)), lookupProof).isDefined }") shouldBe
      (AccessTree +
        perKbCostOf(
          Seq(sizeOf(avlTree), sizeOf(key1), sizeOf(key1), sizeOf(lookupProof)).foldLeft(0L)(_ + _.dataSize),
          avlTreeOp
        )
        + concreteCollCost + constCost + constCost + selectField)
    cost(s"{ $selfTree.remove(keys, lookupProof).isDefined }") shouldBe
      (AccessTree +
        perKbCostOf(
          Seq(sizeOf(avlTree), sizeOf(key1), sizeOf(lookupProof)).foldLeft(0L)(_ + _.dataSize),
          avlTreeOp
        )
        + constCost + selectField)
    cost(s"{ $selfTree.insert(Coll[(Coll[Byte], Coll[Byte])]((key2, key1)), lookupProof).isDefined }") shouldBe
      (AccessTree +
        perKbCostOf(
          Seq(sizeOf(avlTree), sizeOf(key2), sizeOf(key1), sizeOf(lookupProof)).foldLeft(0L)(_ + _.dataSize),
          avlTreeOp
        )
        + concreteCollCost + constCost + constCost + selectField)
  }

  property("Coll operations cost") {
    val coll = "OUTPUTS"
    val nOutputs = tx.outputs.length
    val collBytes = "CONTEXT.headers(0).id"
    cost(s"{ $coll.filter({ (b: Box) => b.value > 1L }).size > 0 }")(
      selectField + lambdaCost +
        (accessBox + extractCost + constCost + comparisonCost + lambdaInvoke) * nOutputs + collToColl + LengthGTConstCost)

    cost(s"{ $coll.flatMap({ (b: Box) => b.propositionBytes }).size > 0 }")(
      lambdaCost + selectField +
          (accessBox + extractCost + lambdaInvoke) * nOutputs + collToColl + LengthGTConstCost)

    cost(s"{ $coll.zip(OUTPUTS).size > 0 }")(
      selectField + accessBox * tx.outputs.length +
        accessBox * nOutputs * 2 + collToColl + LengthGTConstCost)
    cost(s"{ $coll.map({ (b: Box) => b.value })(0) > 0 }") shouldBe
      (lambdaCost + accessBox + extractCost + selectField + (accessBox + extractCost) * tx.outputs.length
        + collToColl + collByIndex + constCost + GTConstCost)
    cost(s"{ $coll.exists({ (b: Box) => b.value > 1L }) }") shouldBe (accessBox + extractCost + GTConstCost)
    cost(s"{ $coll.append(OUTPUTS).size > 0 }") shouldBe
      (selectField + accessBox * tx.outputs.length +
        accessBox * tx.outputs.length * 2 + collToColl + LengthGTConstCost)
    cost(s"{ $coll.indices.size > 0 }") shouldBe
      (selectField + accessBox * tx.outputs.length + selectField + LengthGTConstCost)
    cost(s"{ $collBytes.getOrElse(0, 1.toByte) == 0 }") shouldBe
      (AccessHeaderCost + selectField + collByIndex + collByIndex + comparisonCost + constCost)
    cost(s"{ $coll.fold(0L, { (acc: Long, b: Box) => acc + b.value }) > 0 }") shouldBe
      ((accessBox + selectField + selectField + extractCost + plusMinus) * tx.outputs.length
        + plusMinus + GTConstCost)
    cost(s"{ $coll.forall({ (b: Box) => b.value > 1L }) }") shouldBe (accessBox + extractCost + GTConstCost)
    cost(s"{ $coll.slice(0, 1).size > 0 }") shouldBe
      (selectField + collToColl + accessBox * tx.outputs.length + LengthGTConstCost)
    cost(s"{ $coll.append(OUTPUTS).size > 0 }") shouldBe
      (selectField + accessBox * tx.outputs.length +
        accessBox * tx.outputs.length * 2 + collToColl + LengthGTConstCost)
    cost(s"{ $collBytes.patch(1, Coll(3.toByte), 1).size > 0 }") shouldBe
      (AccessHeaderCost + constCost + constCost + concreteCollCost + selectField + collToColl + LengthGTConstCost)
    cost(s"{ $collBytes.updated(0, 1.toByte).size > 0 }") shouldBe
      (AccessHeaderCost + selectField + collToColl + LengthGTConstCost)
    cost(s"{ $collBytes.updateMany(Coll(0), Coll(1.toByte)).size > 0 }") shouldBe
      (AccessHeaderCost + selectField + concreteCollCost + concreteCollCost + constCost + constCost + collToColl + LengthGTConstCost)
  }

  property("Option operations cost") {
    val opt = "SELF.R5[Int]"
    val accessOpt = accessBox + accessRegister
    cost(s"{ $opt.get > 0 }")(accessOpt + selectField + GTConstCost)
    cost(s"{ $opt.isDefined }")(accessOpt + selectField)
    cost(s"{ $opt.getOrElse(1) > 0 }")(accessOpt + selectField + constCost + GTConstCost)
    cost(s"{ $opt.filter({ (x: Int) => x > 0}).isDefined }")(
      accessOpt + OptionOp + lambdaCost + GTConstCost + selectField)
    cost(s"{ $opt.map({ (x: Int) => x + 1}).isDefined }")(
      accessOpt + OptionOp + lambdaCost + plusMinus + constCost + selectField)
  }
}