package sigmastate.utxo.examples

import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{TaggedByteArray, ByteArrayConstant, IntConstant, CollectionConstant}
import sigmastate._
import sigmastate.helpers.{SigmaTestingCommons, ErgoProvingInterpreter}
import sigmastate.utxo.{ErgoContext, Height, ErgoInterpreter}
import sigmastate.lang.Terms._

class AtomicSwapExampleSpecification extends SigmaTestingCommons {

  /**
    * Atomic cross-chain trading example:
    * Alice(A) has coins in chain 1, Bob(B) has coins in chain 2, they want to exchange them atomically and with no
    * any trusted mediate.
    *
    * Alternative protocol for Bitcoin is described in this forum message,
    * https://bitcointalk.org/index.php?topic=193281.msg2224949#msg2224949,
    * this implementation is simpler. In particular, only one transaction(one output) is required per party.
    */
  property("atomic cross-chain trading") {
    val proverA = new ErgoProvingInterpreter
    val proverB = new ErgoProvingInterpreter
    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val verifier = new ErgoInterpreter

    val x = proverA.contextExtenders(1).value.asInstanceOf[Array[Byte]]
    val hx = ByteArrayConstant(Blake2b256(x))

    val height1 = 100000
    val height2 = 50000

    val deadlineA = 1000
    val deadlineB = 500

    val env = Map(
      "height1" -> height1, "height2" -> height2,
      "deadlineA" -> deadlineA, "deadlineB" -> deadlineB,
      "pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "hx" -> hx)
    val prop1 = compile(env,
      """{
        |  anyOf(Array(
        |    HEIGHT > height1 + deadlineA && pubkeyA,
        |    pubkeyB && blake2b256(getVar[Array[Byte]](1)) == hx
        |  ))
        |}""".stripMargin).asBoolValue

    //chain1 script
    val prop1Tree = OR(
      AND(GT(Height, Plus(IntConstant(height1), IntConstant(deadlineA))), pubkeyA),
      AND(pubkeyB, EQ(CalcBlake2b256(TaggedByteArray(1)), hx))
    )
    prop1 shouldBe prop1Tree

    val prop2 = compile(env,
      """{
        |  anyOf(Array(
        |    HEIGHT > height2 + deadlineB && pubkeyB,
        |    pubkeyA && blake2b256(getVar[Array[Byte]](1)) == hx
        |  ))
        |}
      """.stripMargin).asBoolValue

    //chain2 script
    val prop2Tree = OR(
      AND(GT(Height, Plus(IntConstant(height2), IntConstant(deadlineB))), pubkeyB),
      AND(pubkeyA, EQ(CalcBlake2b256(TaggedByteArray(1)), hx))
    )
    prop2 shouldBe prop2Tree

    //Preliminary checks:

    //B can't spend coins of A in chain1 (generate a valid proof)
    val ctxf1 = ErgoContext(
      currentHeight = height1 + 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)
    proverB.prove(prop1, ctxf1, fakeMessage).isSuccess shouldBe false

    //A can't withdraw her coins in chain1 (generate a valid proof)
    proverA.prove(prop1, ctxf1, fakeMessage).isSuccess shouldBe false

    //B cant't withdraw his coins in chain2 (generate a valid proof)
    val ctxf2 = ErgoContext(
      currentHeight = height2 + 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(), spendingTransaction = null, self = fakeSelf)
    proverB.prove(prop2, ctxf2, fakeMessage).isSuccess shouldBe false

    //Successful run below:

    //A spends coins of B in chain2
    val ctx1 = ErgoContext(
      currentHeight = height2 + 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)
    val pr = proverA.prove(prop2, ctx1, fakeMessage).get
    verifier.verify(prop2, ctx1, pr, fakeMessage).get._1 shouldBe true

    //B extracts preimage x of hx
    val t = pr.extension.values(1)
    val proverB2 = proverB.withContextExtender(1, t.asInstanceOf[CollectionConstant[SByte.type]])

    //B spends coins of A in chain1 with knowledge of x
    val ctx2 = ErgoContext(
      currentHeight = height1 + 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)
    val pr2 = proverB2.prove(prop1, ctx2, fakeMessage).get
    verifier.verify(prop1, ctx2, pr2, fakeMessage).get._1 shouldBe true
  }

}
