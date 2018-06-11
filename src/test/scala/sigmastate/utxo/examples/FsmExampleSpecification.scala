package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox.{R3, R4}
import org.ergoplatform._
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Lookup}
import scorex.crypto.hash
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.Values._
import sigmastate._
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}
import sigmastate.serialization.ValueSerializer
import sigmastate.utxo._


class FsmExampleSpecification extends SigmaTestingCommons {


  /**
    * Similarly to MAST example, we can do more complex contracts, e.g. ones with cycles. For example, we can do a
    * contract described as a finite state machine.
    * Let's consider that a input in a finite state machine is its script, or its hash. Then assume a machine is
    * described as a table of transitions, an example is below:
    * state1 | script_hash1 | state2
    * state2 | script_hash3 | state3
    * state2 | script_hash2 | state1
    * state3 | script_hash4 | -
    *
    * where state3 is a final state, which denotes end of a contract execution (and by executing a script with the hash
    * script_hash4 it is possible to create any box as a result).
    */
  property("simple FSM example"){
    //backer's prover with his private key
    val prover = new ErgoLikeProvingInterpreter

    val script1 = prover.dlogSecrets.head.publicImage
    val script2 = prover.dhSecrets.head.publicImage
    val script3 = AND(script1, script2)
    val script4 = prover.dlogSecrets.tail.head.publicImage  //a script to leave FSM

    val script1Hash = hash.Blake2b256(ValueSerializer.serialize(script1))
    val script2Hash = hash.Blake2b256(ValueSerializer.serialize(script2))
    val script3Hash = hash.Blake2b256(ValueSerializer.serialize(script3))
    val script4Hash = hash.Blake2b256(ValueSerializer.serialize(script4))

    val state1Id = 1: Byte
    val state2Id = 2: Byte
    val state3Id = 3: Byte

    //artificial state id to mark the fact of leaving the FSM
    val leaveFsmStateId = 0: Byte

    val transition12 = Array(state1Id, state2Id)
    val transition21 = Array(state2Id, state1Id)
    val transition23 = Array(state2Id, state3Id)
    val transition30 = Array(state3Id, leaveFsmStateId)

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 34, valueLengthOpt = Some(0))
    avlProver.performOneOperation(Insert(ADKey @@ (transition12 ++ script1Hash), ADValue @@ Array.emptyByteArray))
    avlProver.performOneOperation(Insert(ADKey @@ (transition21 ++ script2Hash), ADValue @@ Array.emptyByteArray))
    avlProver.performOneOperation(Insert(ADKey @@ (transition23 ++ script3Hash), ADValue @@ Array.emptyByteArray))
    avlProver.performOneOperation(Insert(ADKey @@ (transition30 ++ script4Hash), ADValue @@ Array.emptyByteArray))
    avlProver.generateProof()

    val digest = avlProver.digest
    val treeData = new AvlTreeData(digest, 34, Some(0))

    val fsmDescRegister = R3
    val currentStateRegister = R4

    val scriptVarId = 2: Byte
    val transitionProofId = 3: Byte

    val isMember = IsMember(ExtractRegisterAs(Self, fsmDescRegister),
        Append(
          ConcreteCollection[SByte.type](
            ExtractRegisterAs[SByte.type](Self, currentStateRegister),
            ExtractRegisterAs[SByte.type](ByIndex(Outputs, IntConstant.zero),
                                          currentStateRegister,
                                          Some(ByteConstant(-1 :Byte)))),
          CalcBlake2b256(TaggedByteArray(scriptVarId))
        ),
        TaggedByteArray(transitionProofId))

    val scriptPreservation = EQ(ExtractScriptBytes(ByIndex(Outputs, IntConstant.zero)), ExtractScriptBytes(Self))

    val treePreservation = EQ(
      ExtractRegisterAs[SAvlTree.type](ByIndex(Outputs, IntConstant.zero),
                                        fsmDescRegister,
                                        Some(AvlTreeConstant(AvlTreeData.dummy))),
      ExtractRegisterAs[SAvlTree.type](Self, fsmDescRegister))

    val preservation = AND(scriptPreservation, treePreservation)

    val finalStateCheck = EQ(ExtractRegisterAs[SByte.type](Self, currentStateRegister), ByteConstant(state3Id))
    val finalScriptCorrect = IsMember(ExtractRegisterAs(Self, fsmDescRegister),
      Append(
        ConcreteCollection[SByte.type](ExtractRegisterAs[SByte.type](Self, currentStateRegister),
                                       ByteConstant(leaveFsmStateId)),
        CalcBlake2b256(TaggedByteArray(scriptVarId))
      ),
      TaggedByteArray(transitionProofId))


    val fsmScript = OR(
      AND(isMember, DeserializeContext(scriptVarId, SBoolean), preservation),
      AND(finalStateCheck, finalScriptCorrect, DeserializeContext(scriptVarId, SBoolean))
    )


    //creating a box in an initial state

    val fsmBox1 = ErgoBox(100, fsmScript, Map(fsmDescRegister -> AvlTreeConstant(treeData),
                                             currentStateRegister -> ByteConstant(state1Id)))

    //successful transition from state1 to state2
    val fsmBox2 = ErgoBox(100, fsmScript, Map(fsmDescRegister -> AvlTreeConstant(treeData),
                                             currentStateRegister -> ByteConstant(state2Id)))

    avlProver.performOneOperation(Lookup(ADKey @@ (transition12 ++ script1Hash)))
    val transition12Proof = avlProver.generateProof()

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(fsmBox1),
      ErgoLikeTransaction(IndexedSeq(), IndexedSeq(fsmBox2)),
      self = fsmBox1)

    val spendingProof = prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script1)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition12Proof))
      .prove(fsmScript, ctx, fakeMessage).get

    (new ErgoLikeInterpreter).verify(fsmScript, ctx, spendingProof, fakeMessage).get._1 shouldBe true

    //successful transition back from state2 to state1

    avlProver.performOneOperation(Lookup(ADKey @@ (transition21 ++ script2Hash)))
    val transition21Proof = avlProver.generateProof()

    val ctx2 = ErgoLikeContext(
      currentHeight = 51,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(fsmBox2),
      ErgoLikeTransaction(IndexedSeq(), IndexedSeq(fsmBox1)),
      self = fsmBox2)

    val spendingProof2 = prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script2)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition21Proof))
      .prove(fsmScript, ctx2, fakeMessage).get

    (new ErgoLikeInterpreter).verify(fsmScript, ctx2, spendingProof2, fakeMessage).get._1 shouldBe true


    //Box for state3

    val fsmBox3 = ErgoBox(100, fsmScript, Map(fsmDescRegister -> AvlTreeConstant(treeData),
      currentStateRegister -> ByteConstant(state3Id)))

    //transition from state1 to state3 is impossible

    val transition13 = Array(state1Id, state3Id)

    avlProver.performOneOperation(Lookup(ADKey @@ (transition13 ++ script1Hash)))
    val transition13Proof = avlProver.generateProof()

    val ctx3 = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(fsmBox1),
      ErgoLikeTransaction(IndexedSeq(), IndexedSeq(fsmBox3)),
      self = fsmBox1)

    //honest prover fails
    prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script1)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition13Proof))
      .prove(fsmScript, ctx3, fakeMessage)
      .isSuccess shouldBe false

    //prover tries to transit to state3 from state2 by presenting proof for state1 -> state2
    //honest prover fails
    prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script1)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition12Proof))
      .prove(fsmScript, ctx3, fakeMessage)
      .isSuccess shouldBe false

    //successful transition from state2 to state3
    avlProver.performOneOperation(Lookup(ADKey @@ (transition23 ++ script3Hash)))
    val transition23Proof = avlProver.generateProof()

    val ctx23 = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(fsmBox2),
      ErgoLikeTransaction(IndexedSeq(), IndexedSeq(fsmBox3)),
      self = fsmBox2)

    val spendingProof23 = prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script3)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition23Proof))
      .prove(fsmScript, ctx23, fakeMessage).get

    (new ErgoLikeInterpreter).verify(fsmScript, ctx23, spendingProof23, fakeMessage).get._1 shouldBe true

    //clearing FSM out of the box in the final state

    val freeBox = ErgoBox(100, TrueLeaf)

    avlProver.performOneOperation(Lookup(ADKey @@ (transition30 ++ script4Hash)))
    val transition30Proof = avlProver.generateProof()

    val ctx30 = ErgoLikeContext(
      currentHeight = 52,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(fsmBox3),
      ErgoLikeTransaction(IndexedSeq(), IndexedSeq(freeBox)),
      self = fsmBox3)

    val spendingProof30 = prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script4)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition30Proof))
      .prove(fsmScript, ctx30, fakeMessage).get

    (new ErgoLikeInterpreter).verify(fsmScript, ctx30, spendingProof30, fakeMessage).get._1 shouldBe true

    //it is impossible to leave FSM at state2
    val ctx20 = ErgoLikeContext(
      currentHeight = 52,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(fsmBox2),
      ErgoLikeTransaction(IndexedSeq(), IndexedSeq(freeBox)),
      self = fsmBox2)

    //honest prover fails
    prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script4)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition30Proof))
      .prove(fsmScript, ctx20, fakeMessage)
      .isSuccess shouldBe false
  }
}
