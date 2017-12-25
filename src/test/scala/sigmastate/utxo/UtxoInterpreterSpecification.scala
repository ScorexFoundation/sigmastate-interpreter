package sigmastate.utxo

import com.google.common.primitives.Bytes
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scapi.sigma.DLogProtocol.ProveDlog
import scapi.sigma.ProveDiffieHellmanTuple
import scorex.crypto.encode.Base16
import scorex.crypto.hash.{Blake2b256, Blake2b256Unsafe, Digest32}
import sigmastate._
import BoxHelpers.boxWithMetadata
import edu.biu.scapi.primitives.dlog.GroupElement
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Lookup}
import sigmastate.utxo.SigmaStateBox.{R3, R4}


class UtxoInterpreterSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {
  
  implicit def grElemConvert(leafConstant: GroupElementConstant): GroupElement = leafConstant.value
  implicit def grLeafConvert(elem: GroupElement): Value[SGroupElement.type] = GroupElementConstant(elem)


  private val fakeSelf = boxWithMetadata(0, TrueLeaf)

  //fake message, in a real-life a message is to be derived from a spending transaction
  val fakeMessage = Blake2b256("Hello World")

  property("PropLeaf EQ/NEQ") {
    val prover1 = new UtxoProvingInterpreter
    val prover2 = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val h1 = prover1.dlogSecrets.head.publicImage
    val h2 = prover2.dlogSecrets.head.publicImage

    val ctx = UtxoContext(currentHeight = 0, IndexedSeq(), spendingTransaction = null, self = boxWithMetadata(0, TrueLeaf))


    verifier.reduceToCrypto(EQ(PropLeafConstant(h1), PropLeafConstant(h1)), ctx)
      .get.isInstanceOf[TrueLeaf.type] shouldBe true

    verifier.reduceToCrypto(EQ(PropLeafConstant(h1), PropLeafConstant(h2)), ctx)
      .get.isInstanceOf[FalseLeaf.type] shouldBe true
  }

  //todo: implement
  ignore("TxHasOutput reductions") {}

  /**
    * Crowdfunding example:
    * a project declares a need to raise "minToRaise" amount of tokens until some "timeout" height
    * a backer then creates an output which is spendable by with project's public key until timeout and only if a spending
    * transaction creates an output to project's public key with amount >= minToRaise
    * after the timeout output could be spent by backer only
    */
  property("Evaluation - Crowdfunding Example") {

    //a blockchain node verifying a block containing a spending transaction
    val verifier = new UtxoInterpreter

    //backer's prover with his private key
    val backerProver = new UtxoProvingInterpreter

    //project's prover with his private key
    val projectProver = new UtxoProvingInterpreter

    val backerPubKey = backerProver.dlogSecrets.head.publicImage
    val projectPubKey = projectProver.dlogSecrets.head.publicImage

    val timeout = IntLeafConstant(100)
    val minToRaise = IntLeafConstant(1000)

    // (height >= timeout /\ dlog_g backerKey) \/ (height < timeout /\ dlog_g projKey /\ has_output(amount >= minToRaise, proposition = dlog_g projKey)
    val crowdFundingScript = OR(
      AND(GE(Height, timeout), backerPubKey),
      AND(
        Seq(
          LT(Height, timeout),
          projectPubKey,
          Exists(Outputs, 21, GE(ExtractAmount(TaggedBoxLeaf(21)), minToRaise),
                              EQ(ExtractScript(TaggedBoxLeaf(21)), PropLeafConstant(projectPubKey)))
        )
      )
    )

    val outputToSpend = SigmaStateBox(10, crowdFundingScript)
    val outputWithMetadata = BoxWithMetadata(outputToSpend, BoxMetadata(0, 0))

    //First case: height < timeout, project is able to claim amount of tokens not less than required threshold

    val tx1Output1 = SigmaStateBox(minToRaise.value, projectPubKey)
    val tx1Output2 = SigmaStateBox(1, projectPubKey)

    //normally this transaction would invalid, but we're not checking it in this test
    val tx1 = SigmaStateTransaction(IndexedSeq(), IndexedSeq(tx1Output1, tx1Output2))

    val ctx1 = UtxoContext(currentHeight = timeout.value - 1, IndexedSeq(), spendingTransaction = tx1, self = outputWithMetadata)

    //project is generating a proof and it is passing verification
    val proofP = projectProver.prove(crowdFundingScript, ctx1, fakeMessage).get.proof
    verifier.verify(crowdFundingScript, ctx1, proofP, fakeMessage).get shouldBe true

    //backer can't generate a proof
    backerProver.prove(crowdFundingScript, ctx1, fakeMessage).isFailure shouldBe true


    //Second case: height < timeout, project is NOT able to claim amount of tokens not less than required threshold

    val tx2Output1 = SigmaStateBox(minToRaise.value - 1, projectPubKey)
    val tx2Output2 = SigmaStateBox(1, projectPubKey)
    val tx2 = SigmaStateTransaction(IndexedSeq(), IndexedSeq(tx2Output1, tx2Output2))

    val ctx2 = UtxoContext(currentHeight = timeout.value - 1, IndexedSeq(), spendingTransaction = tx2, self = outputWithMetadata)

    //project cant' generate a proof
    val proofP2Try = projectProver.prove(crowdFundingScript, ctx2, fakeMessage)
    proofP2Try.isSuccess shouldBe false

    //backer can't generate a proof
    val proofB2Try = backerProver.prove(crowdFundingScript, ctx2, fakeMessage)
    proofB2Try.isSuccess shouldBe false

    //Third case: height >= timeout

    //project raised enough money but too late...
    val tx3Output1 = SigmaStateBox(minToRaise.value + 1, projectPubKey)
    val tx3Output2 = SigmaStateBox(1, projectPubKey)
    val tx3 = SigmaStateTransaction(IndexedSeq(), IndexedSeq(tx3Output1, tx3Output2))

    val ctx3 = UtxoContext(currentHeight = timeout.value, IndexedSeq(), spendingTransaction = tx3, self = outputWithMetadata)

    //project cant' generate a proof
    projectProver.prove(crowdFundingScript, ctx3, fakeMessage).isFailure shouldBe true

    //backer is generating a proof and it is passing verification
    val proofB = backerProver.prove(crowdFundingScript, ctx3, fakeMessage).get.proof
    verifier.verify(crowdFundingScript, ctx3, proofB, fakeMessage).get shouldBe true
  }


  /**
    * Demurrage currency example.
    *
    * The idea is that miners enforce users to combine a guarding script of a user ("regular_script") with a condition
    * that anyone (presumably, a miner) can spend no more "demurrage_cost" amount of tokens from an output of the user
    * after "demurrage_period" blocks since output creation. If the user is relocating the money from the output before
    * that height, the miner can charge according to output lifetime.
    *
    * (regular_script) ∨
    * (height > (out.height + demurrage_period ) ∧ has_output(value >= out.value − demurrage_cost, script = out.script))
    */
  property("Evaluation - Demurrage Example") {
    val demurragePeriod = 100
    val demurrageCost = 2

    //a blockchain node veryfing a block containing a spending transaction
    val verifier = new UtxoInterpreter

    //backer's prover with his private key
    val userProver = new UtxoProvingInterpreter

    val regScript = userProver.dlogSecrets.head.publicImage

    val script = OR(
      regScript,
      AND(
        GE(Height, Plus(ExtractHeight(Self), IntLeafConstant(demurragePeriod))),
        Exists(Outputs, 21, GE(ExtractAmount(TaggedBoxLeaf(21)),
                                Minus(ExtractAmount(Self), IntLeafConstant(demurrageCost))),
                            EQ(ExtractScript(TaggedBoxLeaf(21)), ExtractScript(Self)))
      )
    )

    val outHeight = 100
    val outValue = 10

    //case 1: demurrage time hasn't come yet
    val tx1 = SigmaStateTransaction(IndexedSeq(), IndexedSeq(SigmaStateBox(outValue, script)))

    val ctx1 = UtxoContext(
      currentHeight = outHeight + demurragePeriod - 1,
      IndexedSeq(),
      spendingTransaction = tx1,
      self = boxWithMetadata(outValue, script, outHeight))

    //user can spend all the money
    val uProof1 = userProver.prove(script, ctx1, fakeMessage).get.proof
    verifier.verify(script, ctx1, uProof1, fakeMessage).get shouldBe true

    //miner can't spend any money
    verifier.verify(script, ctx1, NoProof, fakeMessage).get shouldBe false

    //case 2: demurrage time has come
    val ctx2 = UtxoContext(
      currentHeight = outHeight + demurragePeriod,
      IndexedSeq(),
      spendingTransaction = tx1,
      self = boxWithMetadata(outValue, script, outHeight))

    //user can spend all the money
    val uProof2 = userProver.prove(script, ctx1, fakeMessage).get.proof
    verifier.verify(script, ctx2, uProof2, fakeMessage).get shouldBe true

    //miner can spend "demurrageCost" tokens
    val tx3 = SigmaStateTransaction(IndexedSeq(), IndexedSeq(SigmaStateBox(outValue - demurrageCost, script)))
    val ctx3 = UtxoContext(
      currentHeight = outHeight + demurragePeriod,
      IndexedSeq(),
      spendingTransaction = tx3,
      self = boxWithMetadata(outValue, script, outHeight))


    assert(ctx3.spendingTransaction.newBoxes.head.propositionBytes sameElements ctx3.self.box.propositionBytes)

    verifier.verify(script, ctx3, NoProof, fakeMessage).get shouldBe true

    //miner can't spend more
    val tx4 = SigmaStateTransaction(IndexedSeq(), IndexedSeq(SigmaStateBox(outValue - demurrageCost - 1, script)))
    val ctx4 = UtxoContext(
      currentHeight = outHeight + demurragePeriod,
      IndexedSeq(),
      spendingTransaction = tx4,
      self = boxWithMetadata(outValue, script, outHeight))

    verifier.verify(script, ctx4, NoProof, fakeMessage).get shouldBe false

    //miner can spend less
    val tx5 = SigmaStateTransaction(IndexedSeq(), IndexedSeq(SigmaStateBox(outValue - demurrageCost + 1, script)))
    val ctx5 = UtxoContext(
      currentHeight = outHeight + demurragePeriod,
      IndexedSeq(),
      spendingTransaction = tx5,
      self = boxWithMetadata(outValue, script, outHeight))

    verifier.verify(script, ctx5, NoProof, fakeMessage).get shouldBe true
  }

  /**
    * The script is asking for a hash function preimage. The "proof" could be replayed, so not really a proof.
    */
  property("prover enriching context") {
    val prover = new UtxoProvingInterpreter
    val preimage = prover.contextExtenders(1: Byte).value
    val prop = EQ(CalcBlake2b256(TaggedByteArray(1)), ByteArrayLeafConstant(Blake2b256(preimage)))

    val ctx = UtxoContext(currentHeight = 0, IndexedSeq(), spendingTransaction = null, self = boxWithMetadata(0, TrueLeaf))
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new UtxoInterpreter
    verifier.verify(prop, ctx, pr.proof, fakeMessage).get shouldBe false //context w/out extensions
    verifier.verify(prop, ctxv, pr.proof, fakeMessage).get shouldBe true
  }

  property("prover enriching context 2") {
    val prover = new UtxoProvingInterpreter
    val preimage1 = prover.contextExtenders(1).value
    val preimage2 = prover.contextExtenders(2).value
    val prop = EQ(CalcBlake2b256(AppendBytes(TaggedByteArray(2), TaggedByteArray(1))),
                  ByteArrayLeafConstant(Blake2b256(preimage2 ++ preimage1)))

    val ctx = UtxoContext(currentHeight = 0, IndexedSeq(), spendingTransaction = null, self = boxWithMetadata(0, TrueLeaf))
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new UtxoInterpreter
    verifier.verify(prop, ctx, pr.proof, fakeMessage).get shouldBe false //context w/out extensions
    verifier.verify(prop, ctxv, pr.proof, fakeMessage).get shouldBe true
  }

  property("prover enriching context - xor") {
    val v1 = Base16.decode("abcdef7865")
    val k1 = 21: Byte

    val v2 = Base16.decode("10abdca345")
    val k2 = 22: Byte

    val r = Base16.decode("bb6633db20")

    val prover = new UtxoProvingInterpreter()
      .withContextExtender(k1, ByteArrayLeafConstant(v1))
      .withContextExtender(k2, ByteArrayLeafConstant(v2))

    val prop = EQ(Xor(TaggedByteArray(k1), TaggedByteArray(k2)), ByteArrayLeafConstant(r))

    val ctx = UtxoContext(currentHeight = 0, IndexedSeq(), spendingTransaction = null, self = boxWithMetadata(0, TrueLeaf))
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new UtxoInterpreter
    verifier.verify(prop, ctx, pr.proof, fakeMessage).get shouldBe false //context w/out extensions
    verifier.verify(prop, ctxv, pr.proof, fakeMessage).get shouldBe true
  }

  property("context enriching mixed w. crypto") {
    val prover = new UtxoProvingInterpreter
    val preimage = prover.contextExtenders(1).value
    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = AND(
      pubkey,
      EQ(CalcBlake2b256(TaggedByteArray(1)), ByteArrayLeafConstant(Blake2b256(preimage)))
    )

    val ctx = UtxoContext(currentHeight = 0, IndexedSeq(), spendingTransaction = null, self = boxWithMetadata(0, TrueLeaf))
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    pr.proof.isInstanceOf[SchnorrNode] shouldBe true

    val verifier = new UtxoInterpreter
    verifier.verify(prop, ctx, pr.proof, fakeMessage).getOrElse(false) shouldBe false //context w/out extensions
    verifier.verify(prop, ctxv, pr.proof, fakeMessage).get shouldBe true
  }

  property("context enriching mixed w. crypto 2") {
    val prover = new UtxoProvingInterpreter
    val preimage1 = prover.contextExtenders(1).value
    val preimage2 = prover.contextExtenders(2).value
    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = AND(
      pubkey,
      EQ(
        CalcBlake2b256(AppendBytes(TaggedByteArray(1), TaggedByteArray(2))),
        ByteArrayLeafConstant(Blake2b256(preimage1 ++ preimage2))
      )
    )

    val ctx = UtxoContext(currentHeight = 0, IndexedSeq(), spendingTransaction = null, self = boxWithMetadata(0, TrueLeaf))
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    pr.proof.isInstanceOf[SchnorrNode] shouldBe true

    val verifier = new UtxoInterpreter
    verifier.verify(prop, ctx, pr.proof, fakeMessage).getOrElse(false) shouldBe false //context w/out extensions
    verifier.verify(prop, ctxv, pr.proof, fakeMessage).get shouldBe true
  }

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
    val proverA = new UtxoProvingInterpreter
    val proverB = new UtxoProvingInterpreter
    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val verifier = new UtxoInterpreter

    val x = proverA.contextExtenders(1).value
    val hx = ByteArrayLeafConstant(Blake2b256(x))

    val height1 = 100000
    val height2 = 50000

    val deadlineA = 1000
    val deadlineB = 500

    //chain1 script
    val prop1 = OR(
      AND(GT(Height, IntLeafConstant(height1 + deadlineA)), pubkeyA),
      AND(pubkeyB, EQ(CalcBlake2b256(TaggedByteArray(1)), hx))
    )

    //chain2 script
    val prop2 = OR(
      AND(GT(Height, IntLeafConstant(height2 + deadlineB)), pubkeyB),
      AND(pubkeyA, EQ(CalcBlake2b256(TaggedByteArray(1)), hx))
    )

    //Preliminary checks:

    //B can't spend coins of A in chain1 (generate a valid proof)
    val ctxf1 = UtxoContext(currentHeight = height1 + 1, IndexedSeq(), spendingTransaction = null, self = fakeSelf)
    proverB.prove(prop1, ctxf1, fakeMessage).isSuccess shouldBe false

    //A can't withdraw her coins in chain1 (generate a valid proof)
    println(proverA.prove(prop1, ctxf1, fakeMessage))
    proverA.prove(prop1, ctxf1, fakeMessage).isFailure shouldBe true

    //B cant't withdraw his coins in chain2 (generate a valid proof)
    val ctxf2 = UtxoContext(currentHeight = height2 + 1, IndexedSeq(), spendingTransaction = null, self = fakeSelf)
    proverB.prove(prop2, ctxf2, fakeMessage).isSuccess shouldBe false

    //Successful run below:

    //A spends coins of B in chain2
    val ctx1 = UtxoContext(currentHeight = height2 + 1, IndexedSeq(), spendingTransaction = null, self = fakeSelf)
    val pr = proverA.prove(prop2, ctx1, fakeMessage).get
    verifier.verify(prop2, ctx1, pr, fakeMessage).get shouldBe true

    //B extracts preimage x of hx
    val t = pr.extension.values(1)
    val proverB2 = proverB.withContextExtender(1, t.asInstanceOf[ByteArrayLeafConstant])

    //B spends coins of A in chain1 with knowledge of x
    val ctx2 = UtxoContext(currentHeight = height1 + 1, IndexedSeq(), spendingTransaction = null, self = fakeSelf)
    val pr2 = proverB2.prove(prop1, ctx2, fakeMessage).get
    verifier.verify(prop1, ctx2, pr2, fakeMessage).get shouldBe true
  }

  /**
    * Whether A or B, or both are able to sign a transaction
    */
  property("simplest linear-sized ring signature (1-out-of-2 OR)") {
    val proverA = new UtxoProvingInterpreter
    val proverB = new UtxoProvingInterpreter
    val proverC = new UtxoProvingInterpreter
    val verifier = new UtxoInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage

    val prop = OR(pubkeyA, pubkeyB)

    val ctx = UtxoContext(currentHeight = 1, IndexedSeq(), spendingTransaction = null, self = fakeSelf)

    val prA = proverA.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prA, fakeMessage).get shouldBe true

    val prB = proverB.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prB, fakeMessage).get shouldBe true

    proverC.prove(prop, ctx, fakeMessage).isFailure shouldBe true
  }

  property("simplest linear-sized ring signature (1-out-of-3 OR)") {
    val proverA = new UtxoProvingInterpreter
    val proverB = new UtxoProvingInterpreter
    val proverC = new UtxoProvingInterpreter
    val verifier = new UtxoInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage

    val prop = OR(pubkeyA, pubkeyB, pubkeyC)

    val ctx = UtxoContext(currentHeight = 1, IndexedSeq(), spendingTransaction = null, self = fakeSelf)

    val prA = proverA.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prA, fakeMessage).get shouldBe true

    val prB = proverB.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prB, fakeMessage).get shouldBe true

    val prC = proverC.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prC, fakeMessage).get shouldBe true
  }

  //two secrets are known, nevertheless, one will be simulated
  property("simplest linear-sized ring signature (1-out-of-4 OR), all secrets are known") {
    val proverA = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val pubkeyA1 = proverA.dlogSecrets.head.publicImage
    val pubkeyA2 = proverA.dlogSecrets(1).publicImage
    val pubkeyA3 = proverA.dlogSecrets(2).publicImage
    val pubkeyA4 = proverA.dlogSecrets(3).publicImage

    val prop = OR(Seq(pubkeyA1, pubkeyA2, pubkeyA3, pubkeyA4))

    val ctx = UtxoContext(currentHeight = 1, IndexedSeq(), spendingTransaction = null, self = fakeSelf)

    val prA = proverA.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prA, fakeMessage).get shouldBe true
  }

  property("complex sig scheme - OR of two ANDs") {
    val proverA = new UtxoProvingInterpreter
    val proverB = new UtxoProvingInterpreter
    val proverC = new UtxoProvingInterpreter
    val proverD = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage
    val pubkeyD = proverD.dlogSecrets.head.publicImage

    val prop = OR(AND(pubkeyA, pubkeyB), AND(pubkeyC, pubkeyD))

    val ctx = UtxoContext(currentHeight = 1, IndexedSeq(), spendingTransaction = null, self = fakeSelf)

    proverA.prove(prop, ctx, fakeMessage).isFailure shouldBe true
    proverB.prove(prop, ctx, fakeMessage).isFailure shouldBe true
    proverC.prove(prop, ctx, fakeMessage).isFailure shouldBe true
    proverD.prove(prop, ctx, fakeMessage).isFailure shouldBe true

    val proverAB = proverA.withSecrets(Seq(proverB.dlogSecrets.head))
    val pr = proverAB.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get shouldBe true

    val proverCD = proverC.withSecrets(Seq(proverD.dlogSecrets.head))
    val pr2 = proverCD.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr2, fakeMessage).get shouldBe true
  }

  property("complex sig scheme - OR of AND and OR") {
    val proverA = new UtxoProvingInterpreter
    val proverB = new UtxoProvingInterpreter
    val proverC = new UtxoProvingInterpreter
    val proverD = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage
    val pubkeyD = proverD.dlogSecrets.head.publicImage

    val prop = OR(AND(pubkeyA, pubkeyB), OR(pubkeyC, pubkeyD))

    val ctx = UtxoContext(currentHeight = 1, IndexedSeq(), spendingTransaction = null, self = fakeSelf)

    proverA.prove(prop, ctx, fakeMessage).isFailure shouldBe true
    proverB.prove(prop, ctx, fakeMessage).isFailure shouldBe true

    val prC = proverC.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prC, fakeMessage).get shouldBe true

    val prD = proverD.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prD, fakeMessage).get shouldBe true

    val proverAB = proverA.withSecrets(Seq(proverB.dlogSecrets.head))
    val pr = proverAB.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get shouldBe true
  }

  property("complex sig scheme - AND of two ORs") {
    val proverA = new UtxoProvingInterpreter
    val proverB = new UtxoProvingInterpreter
    val proverC = new UtxoProvingInterpreter
    val proverD = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage
    val pubkeyD = proverD.dlogSecrets.head.publicImage

    val prop = AND(OR(pubkeyA, pubkeyB), OR(pubkeyC, pubkeyD))

    val ctx = UtxoContext(currentHeight = 1, IndexedSeq(), spendingTransaction = null, self = fakeSelf)

    proverA.prove(prop, ctx, fakeMessage).isFailure shouldBe true
    proverB.prove(prop, ctx, fakeMessage).isFailure shouldBe true
    proverC.prove(prop, ctx, fakeMessage).isFailure shouldBe true
    proverD.prove(prop, ctx, fakeMessage).isFailure shouldBe true

    val proverAC = proverA.withSecrets(Seq(proverC.dlogSecrets.head))
    val pr = proverAC.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get shouldBe true

    val proverBD = proverB.withSecrets(Seq(proverD.dlogSecrets.head))
    val pr2 = proverBD.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr2, fakeMessage).get shouldBe true
  }

  property("complex sig scheme - AND of AND and OR") {
    val proverA = new UtxoProvingInterpreter
    val proverB = new UtxoProvingInterpreter
    val proverC = new UtxoProvingInterpreter
    val proverD = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage
    val pubkeyD = proverD.dlogSecrets.head.publicImage

    val prop = AND(AND(pubkeyA, pubkeyB), OR(pubkeyC, pubkeyD))

    val ctx = UtxoContext(currentHeight = 1, IndexedSeq(), spendingTransaction = null, self = fakeSelf)

    proverA.prove(prop, ctx, fakeMessage).isFailure shouldBe true
    proverB.prove(prop, ctx, fakeMessage).isFailure shouldBe true
    proverC.prove(prop, ctx, fakeMessage).isFailure shouldBe true
    proverD.prove(prop, ctx, fakeMessage).isFailure shouldBe true

    val proverAB = proverA.withSecrets(Seq(proverB.dlogSecrets.head))
    proverAB.prove(prop, ctx, fakeMessage).isFailure shouldBe true

    val proverABC = proverAB.withSecrets(Seq(proverC.dlogSecrets.head))
    val prABC = proverABC.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prABC, fakeMessage).get shouldBe true

    val proverABD = proverAB.withSecrets(Seq(proverC.dlogSecrets.head))
    val prABD = proverABD.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prABD, fakeMessage).get shouldBe true
  }

  property("complex sig scheme - OR of two ORs") {
    val proverA = new UtxoProvingInterpreter
    val proverB = new UtxoProvingInterpreter
    val proverC = new UtxoProvingInterpreter
    val proverD = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage
    val pubkeyD = proverD.dlogSecrets.head.publicImage

    val prop = OR(OR(pubkeyA, pubkeyB), OR(pubkeyC, pubkeyD))

    val ctx = UtxoContext(currentHeight = 1, IndexedSeq(), spendingTransaction = null, self = fakeSelf)

    val prA = proverA.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prA, fakeMessage).get shouldBe true

    val prB = proverB.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prB, fakeMessage).get shouldBe true

    val prC = proverC.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prC, fakeMessage).get shouldBe true

    val prD = proverD.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prD, fakeMessage).get shouldBe true
  }

  property("complex sig scheme - OR w. predicate") {
    val proverA = new UtxoProvingInterpreter
    val proverB = new UtxoProvingInterpreter
    val proverC = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage

    val prop = OR(pubkeyA, pubkeyB, GT(Height, IntLeafConstant(500)))

    val ctx1 = UtxoContext(currentHeight = 1, IndexedSeq(), spendingTransaction = null, self = fakeSelf)
    val prA = proverA.prove(prop, ctx1, fakeMessage).get
    verifier.verify(prop, ctx1, prA, fakeMessage).get shouldBe true
    val prB = proverB.prove(prop, ctx1, fakeMessage).get
    verifier.verify(prop, ctx1, prB, fakeMessage).get shouldBe true
    proverC.prove(prop, ctx1, fakeMessage).isFailure shouldBe true

    val ctx2 = UtxoContext(currentHeight = 501, IndexedSeq(), spendingTransaction = null, self = fakeSelf)
    val prC = proverC.prove(prop, ctx2, fakeMessage).get
    verifier.verify(prop, ctx2, prC, fakeMessage).get shouldBe true
  }

  property("complex sig scheme - OR of OR and AND w. predicate") {
    val proverA = new UtxoProvingInterpreter
    val proverB = new UtxoProvingInterpreter
    val proverC = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage

    val prop = OR(OR(pubkeyA, pubkeyB), AND(pubkeyC, GT(Height, IntLeafConstant(500))))

    val ctx1 = UtxoContext(currentHeight = 1, IndexedSeq(), spendingTransaction = null, self = fakeSelf)

    val prA = proverA.prove(prop, ctx1, fakeMessage).get
    verifier.verify(prop, ctx1, prA, fakeMessage).get shouldBe true
    val prB = proverB.prove(prop, ctx1, fakeMessage).get
    verifier.verify(prop, ctx1, prB, fakeMessage).get shouldBe true
    proverC.prove(prop, ctx1, fakeMessage).isFailure shouldBe true


    val ctx2 = UtxoContext(currentHeight = 501, IndexedSeq(), spendingTransaction = null, self = fakeSelf)

    val prA2 = proverA.prove(prop, ctx2, fakeMessage).get
    verifier.verify(prop, ctx2, prA2, fakeMessage).get shouldBe true
    val prB2 = proverB.prove(prop, ctx2, fakeMessage).get
    verifier.verify(prop, ctx2, prB2, fakeMessage).get shouldBe true
    val prC2 = proverC.prove(prop, ctx2, fakeMessage).get
    verifier.verify(prop, ctx2, prC2, fakeMessage).get shouldBe true
  }

  property("DH tuple") {
    val prover = new UtxoProvingInterpreter
    val fakeProver = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val secret = prover.dhSecrets.head

    val ci = secret.commonInput

    val prop = ProveDiffieHellmanTuple(ci.g, ci.h, ci.u, ci.v)
    val wrongProp = ProveDiffieHellmanTuple(ci.g, ci.h, ci.u, ci.u)

    val ctx = UtxoContext(currentHeight = 1, IndexedSeq(), spendingTransaction = null, self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get shouldBe true

    fakeProver.prove(prop, ctx, fakeMessage).isSuccess shouldBe false
    prover.prove(wrongProp, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("DH tuple - simulation") {
    val proverA = new UtxoProvingInterpreter
    val proverB = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubdhB = proverB.dhSecrets.head.publicImage

    val prop = OR(pubkeyA, pubdhB)

    val ctx = UtxoContext(currentHeight = 1, IndexedSeq(), spendingTransaction = null, self = fakeSelf)

    val prA = proverA.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prA, fakeMessage).get shouldBe true
  }

  property("DH tuple and DLOG") {
    val proverA = new UtxoProvingInterpreter
    val proverB = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubdhA = proverA.dhSecrets.head.publicImage

    val prop = AND(pubkeyA, pubdhA)

    val ctx = UtxoContext(currentHeight = 1, IndexedSeq(), spendingTransaction = null, self = fakeSelf)

    val prA = proverA.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prA, fakeMessage).get shouldBe true

    proverB.prove(prop, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("mixing scenario w. timeout") {
    val proverA = new UtxoProvingInterpreter
    val proverB = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyA2 = proverA.dlogSecrets.head.publicImage

    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyB2 = proverB.dlogSecrets.head.publicImage

    val newBox1 = SigmaStateBox(10, pubkeyB2)
    val newBox2 = SigmaStateBox(10, pubkeyA2)

    val newBoxes = IndexedSeq(newBox1, newBox2)

    val properBytes = Bytes.concat(newBoxes.map(_.bytes): _*)

    val properHash = Blake2b256(properBytes)

    val spendingTransaction = SigmaStateTransaction(IndexedSeq(), newBoxes)

    def mixingRequestProp(sender: ProveDlog, timeout: Int) = OR(
      AND(LE(Height, IntLeafConstant(timeout)),
        EQ(CalcBlake2b256(Fold.sumBytes(MapCollection(Outputs, 21, ExtractBytes(TaggedBoxLeaf(21))))),
          ByteArrayLeafConstant(properHash))),
      AND(GT(Height, IntLeafConstant(timeout)), sender)
    )

    val ctx = UtxoContext(currentHeight = 50, IndexedSeq(), spendingTransaction, self = fakeSelf)

    //before timeout
    val prA = proverA.prove(mixingRequestProp(pubkeyA, 100), ctx, fakeMessage).get
    verifier.verify(mixingRequestProp(pubkeyA, 100), ctx, prA, fakeMessage).get shouldBe true
    verifier.verify(mixingRequestProp(pubkeyB, 100), ctx, prA, fakeMessage).get shouldBe true

    //after timeout
    val prA2 = proverA.prove(mixingRequestProp(pubkeyA, 40), ctx, fakeMessage).get
    verifier.verify(mixingRequestProp(pubkeyA, 40), ctx, prA2, fakeMessage).get shouldBe true
    verifier.verify(mixingRequestProp(pubkeyB, 40), ctx, prA2, fakeMessage).isSuccess shouldBe false
  }

  property("map + sum") {
    val prover = new UtxoProvingInterpreter
    val verifier = new UtxoInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = AND(pubkey, GT(Fold.sum(MapCollection(Outputs, 21, ExtractAmount(TaggedBoxLeaf(21)))), IntLeafConstant(20)))

    val newBox1 = SigmaStateBox(11, pubkey)
    val newBox2 = SigmaStateBox(10, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = SigmaStateTransaction(IndexedSeq(), newBoxes)

    val ctx = UtxoContext(currentHeight = 50, IndexedSeq(), spendingTransaction, self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage)
  }

  property("byindex") {
    val prover = new UtxoProvingInterpreter
    val verifier = new UtxoInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = AND(pubkey, GT(ExtractAmount(ByIndex(Outputs, 0)), IntLeafConstant(10)))

    val newBox1 = SigmaStateBox(11, pubkey)
    val newBox2 = SigmaStateBox(10, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = SigmaStateTransaction(IndexedSeq(), newBoxes)

    val ctx = UtxoContext(currentHeight = 50, IndexedSeq(), spendingTransaction, self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage)


    val fProp1 = AND(pubkey, GT(ExtractAmount(ByIndex(Outputs, 0)), IntLeafConstant(11)))
    prover.prove(fProp1, ctx, fakeMessage).isSuccess shouldBe false

    val fProp2 = AND(pubkey, GT(ExtractAmount(ByIndex(Outputs, 1)), IntLeafConstant(11)))
    prover.prove(fProp2, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("sizeof - num of outputs = num of inputs + 1") {
    val prover = new UtxoProvingInterpreter
    val verifier = new UtxoInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = AND(pubkey, EQ(SizeOf(Outputs), Plus(SizeOf(Inputs), IntLeafConstant(1))))

    val newBox1 = SigmaStateBox(11, pubkey)
    val newBox2 = SigmaStateBox(10, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = SigmaStateTransaction(IndexedSeq(), newBoxes)

    val s = BoxWithMetadata(SigmaStateBox(21, pubkey), BoxMetadata(0L, 0))

    val ctx = UtxoContext(currentHeight = 50,
                          boxesToSpend = IndexedSeq(s),
                          spendingTransaction,
                          self = s)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage)


    val fProp = AND(pubkey, EQ(SizeOf(Outputs), SizeOf(Inputs)))
    prover.prove(fProp, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("exists") {
    val prover = new UtxoProvingInterpreter
    val verifier = new UtxoInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = Exists(Outputs, 21, GT(Plus(ExtractAmount(TaggedBoxLeaf(21)), IntLeafConstant(5)), IntLeafConstant(10)))

    val newBox1 = SigmaStateBox(16, pubkey)
    val newBox2 = SigmaStateBox(15, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = SigmaStateTransaction(IndexedSeq(), newBoxes)

    val ctx = UtxoContext(currentHeight = 50, IndexedSeq(), spendingTransaction, self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get shouldBe true
    //todo: finish
  }

  property("forall") {
    val prover = new UtxoProvingInterpreter
    val verifier = new UtxoInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = ForAll(Outputs, 21, EQ(ExtractAmount(TaggedBoxLeaf(21)), IntLeafConstant(10)))

    val newBox1 = SigmaStateBox(10, pubkey)
    val newBox2 = SigmaStateBox(10, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = SigmaStateTransaction(IndexedSeq(), newBoxes)

    val ctx = UtxoContext(currentHeight = 50, IndexedSeq(), spendingTransaction, self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get shouldBe true
    //todo: finish
  }


  property("forall - fail") {
    val prover = new UtxoProvingInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = ForAll(Outputs, 21, EQ(ExtractAmount(TaggedBoxLeaf(21)), IntLeafConstant(10)))

    val newBox1 = SigmaStateBox(10, pubkey)
    val newBox2 = SigmaStateBox(11, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = SigmaStateTransaction(IndexedSeq(), newBoxes)

    val ctx = UtxoContext(currentHeight = 50, IndexedSeq(), spendingTransaction, self = fakeSelf)

    prover.prove(prop, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("counter") {
    val prover = new UtxoProvingInterpreter
    val verifier = new UtxoInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = Exists(Outputs, 21, EQ(ExtractRegisterAs(TaggedBoxLeaf(21), R3),
      Plus(ExtractRegisterAs(Self, R3), IntLeafConstant(1))))

    val newBox1 = SigmaStateBox(10, pubkey, Map(R3 -> IntLeafConstant(3)))
    val newBox2 = SigmaStateBox(10, pubkey, Map(R3 -> IntLeafConstant(6)))
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = SigmaStateTransaction(IndexedSeq(), newBoxes)

    val s = BoxWithMetadata(SigmaStateBox(20, TrueLeaf, Map(R3 -> IntLeafConstant(5))), BoxMetadata(5, 0))

    val ctx = UtxoContext(currentHeight = 50, IndexedSeq(), spendingTransaction, self = s)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get shouldBe true
  }

  property("counter - no register in outputs") {
    val prover = new UtxoProvingInterpreter
    val verifier = new UtxoInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = Exists(Outputs, 21,
      EQ(ExtractRegisterAs(TaggedBoxLeaf(21), R3, default = Some(IntLeafConstant(0L))),
          Plus(ExtractRegisterAs(Self, R3), IntLeafConstant(1))))

    val newBox1 = SigmaStateBox(10, pubkey)
    val newBox2 = SigmaStateBox(10, pubkey, Map(R3 -> IntLeafConstant(6)))
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = SigmaStateTransaction(IndexedSeq(), newBoxes)

    val s = BoxWithMetadata(SigmaStateBox(20, TrueLeaf, Map(R3 -> IntLeafConstant(5))), BoxMetadata(5, 0))

    val ctx = UtxoContext(currentHeight = 50, IndexedSeq(), spendingTransaction, self = s)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get shouldBe true
  }

  property("avl tree - simplest case") {
    val prover = new UtxoProvingInterpreter
    val verifier = new UtxoInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val avlProver = new BatchAVLProver[Digest32, Blake2b256Unsafe](keyLength = 32, None)

    val key = Blake2b256("hello world")
    avlProver.performOneOperation(Insert(ADKey @@ key, ADValue @@ key))
    avlProver.generateProof()

    avlProver.performOneOperation(Lookup(ADKey @@ key))

    val digest = avlProver.digest
    val proof = avlProver.generateProof()

    val treeData = new AvlTreeData(digest, 32, None)

    val prop = IsMember(ExtractRegisterAs(Self, R3),
      ByteArrayLeafConstant(key),
      ByteArrayLeafConstant(proof))

    val newBox1 = SigmaStateBox(10, pubkey)
    val newBoxes = IndexedSeq(newBox1)

    val spendingTransaction = SigmaStateTransaction(IndexedSeq(), newBoxes)

    val s = BoxWithMetadata(SigmaStateBox(20, TrueLeaf, Map(R3 -> AvlTreeLeafConstant(treeData))), BoxMetadata(5, 0))

    val ctx = UtxoContext(currentHeight = 50, IndexedSeq(), spendingTransaction, self = s)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get shouldBe true
  }

  property("avl tree - prover provides proof") {

    val avlProver = new BatchAVLProver[Digest32, Blake2b256Unsafe](keyLength = 32, None)

    val key = Blake2b256("hello world")
    avlProver.performOneOperation(Insert(ADKey @@ key, ADValue @@ key))
    avlProver.generateProof()

    avlProver.performOneOperation(Lookup(ADKey @@ key))

    val digest = avlProver.digest
    val proof = avlProver.generateProof()

    val treeData = new AvlTreeData(digest, 32, None)

    val proofId = 31: Byte

    val prover = new UtxoProvingInterpreter().withContextExtender(proofId, ByteArrayLeafConstant(proof))
    val verifier = new UtxoInterpreter
    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = IsMember(ExtractRegisterAs(Self, R3),
      ExtractRegisterAs(Self, R4),
      TaggedByteArray(proofId))

    val newBox1 = SigmaStateBox(10, pubkey)
    val newBoxes = IndexedSeq(newBox1)

    val spendingTransaction = SigmaStateTransaction(IndexedSeq(), newBoxes)

    val s = BoxWithMetadata(
      SigmaStateBox(20, TrueLeaf, Map(R3 -> AvlTreeLeafConstant(treeData), R4 -> ByteArrayLeafConstant(key))),
      BoxMetadata(5, 0))

    val ctx = UtxoContext(currentHeight = 50, IndexedSeq(), spendingTransaction, self = s)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)
    verifier.verify(prop, ctxv, pr, fakeMessage).get shouldBe true
  }

  property("prove keys from registers") {
    val prover = new UtxoProvingInterpreter
    val verifier = new UtxoInterpreter

    val pubkey1 = prover.dlogSecrets.head.publicImage
    val pubkey2 = prover.dlogSecrets(1).publicImage
    val pubkey3 = prover.dlogSecrets(2).publicImage

    
    val prop = AND(new ProveDlog(ExtractRegisterAs(Self, R3)), new ProveDlog(ExtractRegisterAs(Self, R4)))


    val newBox1 = SigmaStateBox(10, pubkey3)
    val newBoxes = IndexedSeq(newBox1)
    val spendingTransaction = SigmaStateTransaction(IndexedSeq(), newBoxes)

    val s = BoxWithMetadata(SigmaStateBox(20, TrueLeaf, Map(R3 -> pubkey1.value, R4 -> pubkey2.value)), BoxMetadata(5, 0))


    val ctx = UtxoContext(currentHeight = 50, IndexedSeq(), spendingTransaction, self = s)
    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get shouldBe true
  }

  /**
    *
    * An oracle example.
    *
    * A trusted weather station is publishing temperature data on blockchain.
    * Alice and Bob are making a contract based on the data:
    *   they have locked coins in such way that if output from the station shows that
    *   temperature announced by the oracle is > 15 degrees, money are going to Alice, otherwise to Bob.
    *
    * We consider that for validating transaction only limited number of last headers and the spending transaction
    * should be enough, in addition to outputs being spent by the transaction. Thus there is no need for knowledge
    * of an arbitrary output. To show the coin of the weather service in the spending transaction, outputs from Alice
    * and Bob are referencing to the coin by using Merkle proofs against UTXO set root hash of the latest known block.
    *
    * A tricky moment is how Alice and Bob can be sure that a coin is indeed created by the service, having just the
    * coin (and also service's public key x = g^w, where service's secret w is not known.
    *
    *
    * For that, we consider that the service creates a coin with registers of following semantics (R0 & R1 are standard):
    *
    * R0 - coin amount
    * R1 - protecting script, which is the pubkey of the service, x =  g^w
    * R2 - temperature data, number
    * R3 - a = g^r, where r is secret random nonce
    * R4 - z = r + ew mod q
    *
    * Then Alice and Bob are requiring from the coin that the following equation holds:
    * (g^z = a * x^e, where e = hash(R2)
    *
    * Thus Alice, for example, is created a coin with the following statement (we skip timeouts for simplicity):
    * "the coin is spendable if against UTXO set root hash for the last known block there is a coin along with a Merkle
    * proof, for which following requirements hold: R1 = dlog(x) /\ g^(R4) = R3 * x^(hash(R2)) /\ (R2) > 15"
    *
    */
  ignore("oracle example") {

  }
}