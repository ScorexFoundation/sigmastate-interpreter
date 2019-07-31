package special.sigma

import org.ergoplatform.ErgoConstants.ScriptCostLimit
import org.ergoplatform.validation.ValidationRules
import sigmastate.interpreter.ContextExtension
import org.scalacheck.Gen.containerOfN
import sigmastate.{AvlTreeFlags, TrivialProp}
import sigmastate.Values.{BooleanConstant, IntConstant}
import org.scalacheck.{Arbitrary, Gen}
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.eval._
import sigmastate.eval.Extensions._
import org.ergoplatform.{DataInput, ErgoBox, ErgoLikeContext, ErgoLikeTransaction}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.crypto.authds.{ADKey, ADValue}

trait SigmaTestingData extends SigmaTestingCommons with SigmaTypeGens {
  val bytesGen: Gen[Array[Byte]] = containerOfN[Array, Byte](100, Arbitrary.arbByte.arbitrary)
  val bytesCollGen = bytesGen.map(Colls.fromArray(_))
  implicit val arbBytes = Arbitrary(bytesCollGen)
  val keyCollGen = bytesCollGen.map(_.slice(0, 32))
  import org.ergoplatform.dsl.AvlTreeHelpers._

  protected def sampleAvlProver = {
    val key = keyCollGen.sample.get
    val value = bytesCollGen.sample.get
    val (_, avlProver) = createAvlTree(AvlTreeFlags.AllOperationsAllowed, ADKey @@ key.toArray -> ADValue @@ value.toArray)
    (key, value, avlProver)
  }

  protected def sampleAvlTree: AvlTree = {
    val (key, _, avlProver) = sampleAvlProver
    val digest = avlProver.digest.toColl
    val tree = SigmaDsl.avlTree(AvlTreeFlags.ReadOnly.serializeToByte, digest, 32, None)
    tree
  }

  val tokenId1: Digest32 = Blake2b256("id1")
  val tokenId2: Digest32 = Blake2b256("id2")
  val inBox = createBox(10, TrivialProp.TrueProp,
    Seq(tokenId1 -> 10L, tokenId2 -> 20L),
    Map(ErgoBox.R4 -> IntConstant(100), ErgoBox.R5 -> BooleanConstant(true)))

  val dataBox = createBox(1000, TrivialProp.TrueProp,
    Seq(tokenId1 -> 10L, tokenId2 -> 20L),
    Map(ErgoBox.R4 -> IntConstant(100), ErgoBox.R5 -> BooleanConstant(true)))

  val outBox = createBox(10, TrivialProp.TrueProp,
    Seq(tokenId1 -> 10L, tokenId2 -> 20L),
    Map(ErgoBox.R4 -> IntConstant(100), ErgoBox.R5 -> BooleanConstant(true)))

  val header1: Header = CHeader(Blake2b256("Header.id").toColl,
    0,
    Blake2b256("Header.parentId").toColl,
    Blake2b256("ADProofsRoot").toColl,
    sampleAvlTree,
    Blake2b256("transactionsRoot").toColl,
    timestamp = 0,
    nBits = 0,
    height = 0,
    extensionRoot = Blake2b256("transactionsRoot").toColl,
    minerPk = SigmaDsl.groupGenerator,
    powOnetimePk = SigmaDsl.groupGenerator,
    powNonce = Colls.fromArray(Array[Byte](0, 1, 2, 3, 4, 5, 6, 7)),
    powDistance = SigmaDsl.BigInt(BigInt("1405498250268750867257727119510201256371618473728619086008183115260323").bigInteger),
    votes = Colls.fromArray(Array[Byte](0, 1, 2))
  )
  val header2: Header = CHeader(Blake2b256("Header2.id").toColl,
    0,
    header1.id,
    Blake2b256("ADProofsRoot2").toColl,
    sampleAvlTree,
    Blake2b256("transactionsRoot2").toColl,
    timestamp = 2,
    nBits = 0,
    height = 1,
    extensionRoot = Blake2b256("transactionsRoot2").toColl,
    minerPk = SigmaDsl.groupGenerator,
    powOnetimePk = SigmaDsl.groupGenerator,
    powNonce = Colls.fromArray(Array.fill(0.toByte)(8)),
    powDistance = SigmaDsl.BigInt(BigInt("19306206489815517413186395405558417825367537880571815686937307203793939").bigInteger),
    votes =  Colls.fromArray(Array[Byte](0, 1, 0))
  )
  val headers = Colls.fromItems(header2, header1)
  val preHeader: PreHeader = CPreHeader(0,
    header2.id,
    timestamp = 3,
    nBits = 0,
    height = 2,
    minerPk = SigmaDsl.groupGenerator,
    votes = Colls.emptyColl[Byte]
  )
  val ergoCtx = new ErgoLikeContext(
    lastBlockUtxoRoot = header2.stateRoot.asInstanceOf[CAvlTree].treeData,
    boxesToSpend = IndexedSeq(inBox),
    spendingTransaction = new ErgoLikeTransaction(IndexedSeq(), IndexedSeq(DataInput(dataBox.id)), IndexedSeq(outBox)),
    selfIndex = 0, headers = headers, preHeader = preHeader, dataBoxes = IndexedSeq(dataBox),
    extension = ContextExtension.empty,
    validationSettings = ValidationRules.currentSettings,
    costLimit = ScriptCostLimit.value, initCost = 0L)
}
