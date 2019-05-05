package sigmastate

import org.ergoplatform.{ErgoLikeContext, ErgoLikeTransaction}
import sigmastate.Values.{NotReadyValueInt, IntConstant}
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, ErgoLikeTestInterpreter}
import sigmastate.interpreter.Interpreter
import sigmastate.interpreter.Interpreter.ScriptNameProp
import sigmastate.serialization.{SigmaSerializer, OpCodes}
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.{SerializerException, InvalidOpCode}
import sigmastate.serialization.OpCodes.{LastConstantCode, OpCode}
import special.sigma.SigmaTestingData

class SoftForkabilitySpecification extends SigmaTestingData {

  implicit lazy val IR = new TestingIRContext
  lazy val prover = new ErgoLikeTestProvingInterpreter()
  lazy val verifier = new ErgoLikeTestInterpreter
  lazy val invalidPropV1 = compile(Interpreter.emptyEnv,
    """{
     |  HEIGHT > 100 && OUTPUTS.size == 1
     |}""".stripMargin).asBoolValue
  lazy val invalidTxV1 = createTransaction(createBox(100, invalidPropV1.asSigmaProp, 1))
  lazy val invalidTxV1bytes = invalidTxV1.messageToSign

  lazy val propV1 = invalidPropV1.toSigmaProp
  lazy val txV1 = createTransaction(createBox(100, propV1, 1))
  lazy val txV1bytes = txV1.messageToSign

  def createContext(h: Int, tx: ErgoLikeTransaction) =
    ErgoLikeContext(h,
      AvlTreeData.dummy, ErgoLikeContext.dummyPubkey, IndexedSeq(fakeSelf),
      tx, fakeSelf)

  property("node v1, received tx with script v1, incorrect script") {
    an[SerializerException] should be thrownBy {
      // CheckDeserializedScriptIsSigmaProp rule violated
      ErgoLikeTransaction.serializer.parse(SigmaSerializer.startReader(invalidTxV1bytes))
    }
  }

  property("node v1, received tx with script v1, correct script") {
    // able to parse
    val tx = ErgoLikeTransaction.serializer.parse(SigmaSerializer.startReader(txV1bytes))

    // validating script
    val env = Map(ScriptNameProp -> "propV1")
    val ctx = createContext(110, tx)
    val proof1 = prover.prove(env, propV1, ctx, fakeMessage).get.proof
    verifier.verify(env, propV1, ctx, proof1, fakeMessage).map(_._1).getOrElse(false) shouldBe true
  }

  /** Same as Height, but new opcode to test soft-fork */
  case object Height2 extends NotReadyValueInt {
    override val opCode: OpCode = (LastConstantCode + 56).toByte // use reserved code
    def opType = SFunc(SContext, SInt)
  }
  lazy val prop = GT(Height2, IntConstant(100))
  lazy val invalidTxV2 = createTransaction(createBox(100, prop.asSigmaProp, 1))
  lazy val invalidTxV2bytes = invalidTxV2.messageToSign

  lazy val propV2 = prop.toSigmaProp
  lazy val txV2 = createTransaction(createBox(100, propV2, 1))
  lazy val txV2bytes = txV2.messageToSign

  property("node v1, soft-fork up to v2, script v2, parsed ok") {
    // able to parse
    val tx = ErgoLikeTransaction.serializer.parse(SigmaSerializer.startReader(txV2bytes))

    // validating script
    val env = Map(ScriptNameProp -> "propV2")
    val ctx = createContext(110, tx)
    val proof1 = prover.prove(env, propV2, ctx, fakeMessage).get.proof
    verifier.verify(env, propV2, ctx, proof1, fakeMessage).map(_._1).getOrElse(false) shouldBe true
  }

  property("node v1, soft-fork up to v2, script v2, soft-fork exception") {
    // try to parse
    // handle soft-fork exception, skip validation
  }

  property("node v1, no soft-fork, received script v2, raise error") {
    an[InvalidOpCode] should be thrownBy {
      ErgoLikeTransaction.serializer.parse(SigmaSerializer.startReader(invalidTxV2bytes))
    }
  }

  property("our node v2, was soft-fork up to v2, received script v2") {
    // able to parse
    // validating script
  }

}
