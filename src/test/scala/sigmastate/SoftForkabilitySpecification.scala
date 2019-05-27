package sigmastate

import org.ergoplatform.ValidationRules.{CheckDeserializedScriptIsSigmaProp, CheckTupleType, CheckValidOpCode, trySoftForkable}
import org.ergoplatform._
import sigmastate.SPrimType.MaxPrimTypeCode
import sigmastate.Values.{UnparsedErgoTree, NotReadyValueInt, ByteArrayConstant, Tuple, IntConstant, ErgoTree, ValueCompanion}
import sigmastate.eval.Colls
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, ErgoLikeTestInterpreter}
import sigmastate.interpreter.{ProverResult, ContextExtension}
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigmastate.serialization._
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.{SerializerException, CosterException}
import sigmastate.serialization.DataSerializer.CheckSerializableTypeCode
import sigmastate.serialization.OpCodes.{LastConstantCode, OpCode}
import sigmastate.serialization.TypeSerializer.{CheckPrimitiveTypeCode, CheckTypeCode}
import sigmastate.utxo.{DeserializeContext, SelectField}
import special.sigma.SigmaTestingData

class SoftForkabilitySpecification extends SigmaTestingData {

  implicit lazy val IR = new TestingIRContext
  lazy val prover = new ErgoLikeTestProvingInterpreter()
  lazy val verifier = new ErgoLikeTestInterpreter
  val deadline = 100
  val boxAmt = 100L
  lazy val invalidPropV1 = compile(emptyEnv + ("deadline" -> deadline),
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

  def proveTx(name: String, tx: ErgoLikeTransaction, vs: ValidationSettings): ProverResult = {
    val env = Map(ScriptNameProp -> (name + "_prove"))
    val ctx = createContext(blockHeight, tx, vs)
    val prop = tx.outputs(0).ergoTree
    val proof1 = prover.prove(env, prop, ctx, fakeMessage).get
    proof1
  }

  def verifyTx(name: String, tx: ErgoLikeTransaction, proof: ProverResult, vs: ValidationSettings) = {
    val env = Map(ScriptNameProp -> (name + "_verify"))
    val ctx = createContext(blockHeight, tx, vs)
    val prop = tx.outputs(0).ergoTree
    verifier.verify(env, prop, ctx, proof, fakeMessage).map(_._1).fold(t => throw t, identity) shouldBe true
  }

  def proveAndVerifyTx(name: String, tx: ErgoLikeTransaction, vs: ValidationSettings) = {
    val proof = proveTx(name, tx, vs)
    verifyTx(name, tx, proof, vs)
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
  case object Height2 extends NotReadyValueInt with ValueCompanion {
    override def companion = this
    override val opCode: OpCode = Height2Code // use reserved code
    def opType = SFunc(SContext, SInt)
  }
  val Height2Ser = CaseObjectSerialization(Height2, Height2)

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

  val newTypeCode = (SGlobal.typeCode + 1).toByte

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
    // script bytes for context variable containing v2 operation
    val propBytes = runOnV2Node {
      ValueSerializer.serialize(prop)
    }

    // v1 main script which deserializes from context v2 script
    val mainProp = BinAnd(GT(Height, IntConstant(deadline)), DeserializeContext(1, SBoolean)).toSigmaProp

    val tx = createTransaction(createBox(boxAmt, ErgoTree.fromProposition(mainProp), 1))
    val bindings = Map(1.toByte -> ByteArrayConstant(Colls.fromArray(propBytes)))
    val proof = new ProverResult(Array.emptyByteArray, ContextExtension(bindings))

    // verify transaction on v1 node using v2 validation settings
    verifyTx("deserialize", tx, proof, v2vs)
  }

  def checkRule(rule: ValidationRule, v2vs: ValidationSettings, action: => Unit) = {
    // try SoftForkable block using current vs (v1 version)
    assertExceptionThrown({
      trySoftForkable(false) {
        action
        true
      }
    }, {
      case ve: ValidationException if ve.rule == rule => true
      case _ => false
    })

    val res = trySoftForkable(false)({
      action
      true
    })(v2vs)
    res shouldBe false
  }

  property("CheckTupleType rule") {
    val exp = SelectField(Tuple(IntConstant(1), IntConstant(2), IntConstant(3)), 3)
    val v2vs = vs.updated(CheckTupleType.id, ReplacedRule(0))
    checkRule(CheckTupleType, v2vs, {
      IR.doCosting(emptyEnv, exp)
    })
  }

  property("CheckPrimitiveTypeCode rule") {
    val typeBytes = Array[Byte](MaxPrimTypeCode)
    val v2vs = vs.updated(CheckPrimitiveTypeCode.id, ChangedRule(Array[Byte](MaxPrimTypeCode)))
    checkRule(CheckPrimitiveTypeCode, v2vs, {
      val r = SigmaSerializer.startReader(typeBytes)
      TypeSerializer.deserialize(r)
    })
  }

  property("CheckTypeCode rule") {
    val typeBytes = Array[Byte](newTypeCode)
    val v2vs = vs.updated(CheckTypeCode.id, ChangedRule(Array[Byte](newTypeCode)))
    checkRule(CheckTypeCode, v2vs, {
      val r = SigmaSerializer.startReader(typeBytes)
      TypeSerializer.deserialize(r)
    })
  }

  property("CheckSerializableTypeCode rule") {
    val newType = SFunc(SInt, SInt)
    val dataBytes = Array[Byte](1, 2, 3) // any random bytes will work
    val v2vs = vs.updated(CheckSerializableTypeCode.id, ReplacedRule(0))
    checkRule(CheckSerializableTypeCode, v2vs, {
      val r = SigmaSerializer.startReader(dataBytes)
      DataSerializer.deserialize(newType, r)
    })
  }

  property("CheckTypeWithMethods rule") {
    val freeMethodId = 1.toByte
    val mcBytes = Array[Byte](OpCodes.PropertyCallCode, newTypeCode, freeMethodId, Outputs.opCode)
    val v2vs = vs.updated(CheckTypeWithMethods.id, ChangedRule(Array(newTypeCode)))
    checkRule(CheckTypeWithMethods, v2vs, {
      ValueSerializer.deserialize(mcBytes)
    })
  }

  property("CheckMethod rule") {
    val freeMethodId = 16.toByte
    val mcBytes = Array[Byte](OpCodes.PropertyCallCode, SCollection.typeId, freeMethodId, Outputs.opCode)
    val v2vs = vs.updated(CheckMethod.id, ChangedRule(Array(SCollection.typeId, freeMethodId)))
    checkRule(CheckMethod, v2vs, {
      ValueSerializer.deserialize(mcBytes)
    })
  }

}
