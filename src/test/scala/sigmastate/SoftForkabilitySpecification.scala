package sigmastate

import org.ergoplatform.ValidationRules.{CheckValidOpCode, CheckDeserializedScriptIsSigmaProp}
import org.ergoplatform._
import sigmastate.Values.{NotReadyValueInt, ErgoTree, IntConstant, UnparsedErgoTree}
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, ErgoLikeTestInterpreter}
import sigmastate.interpreter.Interpreter
import sigmastate.interpreter.Interpreter.ScriptNameProp
import sigmastate.serialization._
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.{SerializerException, CosterException, InvalidOpCode}
import sigmastate.serialization.OpCodes.{LastConstantCode, OpCode}
import special.sigma.SigmaTestingData

class SoftForkabilitySpecification extends SigmaTestingData {

  implicit lazy val IR = new TestingIRContext
  lazy val prover = new ErgoLikeTestProvingInterpreter()
  lazy val verifier = new ErgoLikeTestInterpreter
  val deadline = 100
  val boxAmt = 100L
  lazy val invalidPropV1 = compile(Interpreter.emptyEnv + ("deadline" -> deadline),
    """{
     |  HEIGHT > deadline && OUTPUTS.size == 1
     |}""".stripMargin).asBoolValue
  lazy val invalidTxV1 = createTransaction(createBox(boxAmt, invalidPropV1.asSigmaProp, 1))
  lazy val invalidTxV1bytes = invalidTxV1.messageToSign

  lazy val propV1 = invalidPropV1.toSigmaProp
  lazy val txV1 = createTransaction(createBox(boxAmt, propV1, 1))
  lazy val txV1bytes = txV1.messageToSign

  val blockHeight = 110

  def createContext(h: Int, tx: ErgoLikeTransaction, vs: ValidationSettings) =
    ErgoLikeContext(h,
      AvlTreeData.dummy, ErgoLikeContext.dummyPubkey, IndexedSeq(fakeSelf),
      tx, fakeSelf, vs = vs)

  def proveAndVerifyTx(name: String, tx: ErgoLikeTransaction, vs: ValidationSettings) = {
    val env = Map(ScriptNameProp -> name)
    val ctx = createContext(blockHeight, tx, vs)
    val prop = tx.outputs(0).ergoTree
    val proof1 = prover.prove(env, prop, ctx, fakeMessage).get.proof
    verifier.verify(env, prop, ctx, proof1, fakeMessage).map(_._1).getOrElse(false) shouldBe true
  }

  def proveTx(name: String, tx: ErgoLikeTransaction, vs: ValidationSettings) = {
    val env = Map(ScriptNameProp -> (name + "_prove"))
    val ctx = createContext(blockHeight, tx, vs)
    val prop = tx.outputs(0).ergoTree
    val proof1 = prover.prove(env, prop, ctx, fakeMessage).get.proof
    proof1
  }

  def verifyTx(name: String, tx: ErgoLikeTransaction, proof: Array[Byte], vs: ValidationSettings) = {
    val env = Map(ScriptNameProp -> (name + "_verify"))
    val ctx = createContext(blockHeight, tx, vs)
    val prop = tx.outputs(0).ergoTree
    verifier.verify(env, prop, ctx, proof, fakeMessage).map(_._1).getOrElse(false) shouldBe true
  }

  def checkTxProp[T <: ErgoLikeTransaction, R](tx1: T, tx2: T)(p: T => R) = {
    p(tx1) shouldBe p(tx2)
  }

  property("node v1, received tx with script v1, incorrect script") {
    assertExceptionThrown({
      // CheckDeserializedScriptIsSigmaProp rule violated
      ErgoLikeTransaction.serializer.parse(SigmaSerializer.startReader(invalidTxV1bytes))
    }, {
      case se: SerializerException if se.cause.isDefined =>
        val ve = se.cause.get.asInstanceOf[ValidationException]
        ve.rule == CheckDeserializedScriptIsSigmaProp
      case _ => false
    })
  }

  property("node v1, received tx with script v1, correct script") {
    // able to parse
    val tx = ErgoLikeTransaction.serializer.parse(SigmaSerializer.startReader(txV1bytes))

    // validating script
    proveAndVerifyTx("propV1", tx, vs)
  }

  val Height2Code = (LastConstantCode + 56).toByte
  /** Same as Height, but new opcode to test soft-fork */
  case object Height2 extends NotReadyValueInt {
    override val opCode: OpCode = Height2Code // use reserved code
    def opType = SFunc(SContext, SInt)
  }
  val Height2Ser = CaseObjectSerialization(Height2Code, Height2)

  // prepare soft-fork settings for v2
  val v2vs = vs.updated(CheckValidOpCode.id, ChangedRule(Array(Height2Code)))

  /** Runs the block as if on the v2 node. */
  def runOnV2Node[T](block: => T): T = {
    ValueSerializer.addSerializer(Height2Code, Height2Ser)
    val res = block
    ValueSerializer.removeSerializer(Height2Code)
    res
  }

  lazy val prop = GT(Height2, IntConstant(deadline))
  lazy val invalidTxV2 = createTransaction(createBox(boxAmt, prop.asSigmaProp, 1))


  lazy val propV2 = prop.toSigmaProp
  // prepare bytes using special serialization WITH `size flag` in the header
  lazy val propV2tree = ErgoTree.withSegregation(ErgoTree.SizeFlag,  propV2)
  lazy val propV2treeBytes = runOnV2Node {
    propV2tree.bytes
  }

  lazy val txV2 = createTransaction(createBox(boxAmt, propV2tree, 1))
  lazy val txV2messageToSign = runOnV2Node {
    txV2.messageToSign
  }

  property("node v1, soft-fork up to v2, script v2 without size") {
    // prepare bytes using default serialization without `size bit` in the header
    val (txV2_withoutSize, txV2_withoutSize_bytes) = runOnV2Node {
      val tx = createTransaction(createBox(boxAmt, ErgoTree.fromProposition(propV2), 1))
      (tx, tx.messageToSign)
    }

    // should fail with given exceptions
    assertExceptionThrown(
      {
        val r = SigmaSerializer.startReader(txV2_withoutSize_bytes)
        ErgoLikeTransaction.serializer.parse(r)
      },
      {
        case se: SerializerException if se.cause.isDefined =>
          val ve = se.cause.get.asInstanceOf[ValidationException]
          ve.rule == CheckValidOpCode
        case _ => false
      }
    )
  }

  property("node v1, soft-fork up to v2, script v2 with `size bit`") {
    val treeBytes = propV2treeBytes
    val txV2bytes = txV2messageToSign

    // parse and validate tx with v2 settings
    val tx = ErgoLikeTransaction.serializer.parse(SigmaSerializer.startReader(txV2bytes))
    proveAndVerifyTx("propV2", tx, v2vs)

    // also check that transaction prop was trivialized due to soft-fork
    tx.outputs(0).ergoTree.root.left.get.bytes.array shouldBe treeBytes
    tx.outputs(0).ergoTree.root.left.get.isInstanceOf[UnparsedErgoTree] shouldBe true

    // check deserialized tx is otherwise remains the same
    checkTxProp(txV2, tx)(_.inputs)
    checkTxProp(txV2, tx)(_.dataInputs)
    checkTxProp(txV2, tx)(_.outputs.length)
    checkTxProp(txV2, tx)(_.outputs(0).creationHeight)
    checkTxProp(txV2, tx)(_.outputs(0).value)
    checkTxProp(txV2, tx)(_.outputs(0).additionalTokens)
  }

  property("node v1, no soft-fork, received script v2, raise error") {
    assertExceptionThrown({
      val invalidTxV2bytes = runOnV2Node { invalidTxV2.messageToSign }
      ErgoLikeTransaction.serializer.parse(SigmaSerializer.startReader(invalidTxV2bytes))
    },{
      case se: SerializerException if se.cause.isDefined =>
        val ve = se.cause.get.asInstanceOf[ValidationException]
        ve.rule == CheckValidOpCode
      case _ => false
    })
  }

  property("our node v2, was soft-fork up to v2, received script v2") {
    val txV2bytes = txV2.messageToSign

    // run as on node v2
    runOnV2Node {

      // parse and validate tx with v2 script (since serializers were extended to v2)
      val tx = ErgoLikeTransaction.serializer.parse(SigmaSerializer.startReader(txV2bytes))
      tx shouldBe txV2

      // fails evaluation of v2 script (due to the rest of the implementation is still v1)
      assertExceptionThrown({
        proveAndVerifyTx("propV2", tx, v2vs)
      },{
        case _: CosterException => true
        case _ => false
      })
    }
  }
  
  property("our node v1, was soft-fork up to v2, received v1 script, DeserializeContext of v2 script") {
    val txV2bytes = txV2.messageToSign

    val prop = GT(Height, IntConstant(deadline)).toSigmaProp
    val tx = createTransaction(createBox(boxAmt, ErgoTree.fromProposition(prop), 1))

  }

}
