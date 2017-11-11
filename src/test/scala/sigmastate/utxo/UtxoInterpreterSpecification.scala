package sigmastate.utxo

import com.google.common.primitives.Bytes
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scapi.sigma.DLogProtocol.DLogNode
import scapi.sigma.DiffieHellmanTupleNode
import scorex.crypto.encode.Base16
import scorex.crypto.hash.Blake2b256
import sigmastate._
import sigmastate.utils.Helpers
import BoxHelpers.boxWithMetadata


class UtxoInterpreterSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  private val fakeSelf = boxWithMetadata(0, TrueLeaf)

  property("PropLeaf EQ/NEQ") {
    val prover1 = new UtxoProvingInterpreter
    val prover2 = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val h1 = prover1.dlogSecrets.head.publicImage.h
    val h2 = prover2.dlogSecrets.head.publicImage.h

    val ctx = UtxoContext(currentHeight = 0, spendingTransaction = null, self = boxWithMetadata(0, TrueLeaf))


    verifier.reduceToCrypto(EQ(PropLeaf(DLogNode(h1)), PropLeaf(DLogNode(h1))), ctx)
      .get.isInstanceOf[TrueLeaf.type] shouldBe true

    verifier.reduceToCrypto(EQ(PropLeaf(DLogNode(h1)), PropLeaf(DLogNode(h2))), ctx)
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

    val backerPubKey = backerProver.dlogSecrets.head.publicImage.h
    val projectPubKey = projectProver.dlogSecrets.head.publicImage.h

    val timeout = NonNegativeIntLeaf(100)
    val minToRaise = NonNegativeIntLeaf(1000)

    // (height >= timeout /\ dlog_g backerKey) \/ (height < timeout /\ dlog_g projKey /\ has_output(amount >= minToRaise, proposition = dlog_g projKey)
    val crowdFundingScript = OR(
      AND(GE(Height, timeout), DLogNode(backerPubKey)),
      AND(
        Seq(
          LT(Height, timeout),
          DLogNode(projectPubKey),
          TxHasOutput(GE(OutputAmount, minToRaise), EQ(OutputScript, PropLeaf(DLogNode(projectPubKey))))
        )
      )
    )

    val outputToSpend = SigmaStateBox(10, crowdFundingScript)
    val outputWithMetadata = BoxWithMetadata(outputToSpend, BoxMetadata(0, 0))
    val message = Blake2b256("Hello World") //normally message to be defined by spending transaction bytes

    //First case: height < timeout, project is able to claim amount of tokens not less than required threshold

    val tx1Output1 = SigmaStateBox(minToRaise.value, DLogNode(projectPubKey))
    val tx1Output2 = SigmaStateBox(1, DLogNode(projectPubKey))

    //normally this transaction would invalid, but we're not checking it in this test
    val tx1 = SigmaStateTransaction(Seq(), Seq(tx1Output1, tx1Output2))

    val ctx1 = UtxoContext(currentHeight = timeout.value - 1, spendingTransaction = tx1, self = outputWithMetadata)

    //project is generating a proof and it is passing verification
    val proofP = projectProver.prove(crowdFundingScript, ctx1, message).get.proof
    verifier.verify(crowdFundingScript, ctx1, proofP, message).get shouldBe true

    //backer can't generate a proof
    backerProver.prove(crowdFundingScript, ctx1, message).isFailure shouldBe true


    //Second case: height < timeout, project is NOT able to claim amount of tokens not less than required threshold

    val tx2Output1 = SigmaStateBox(minToRaise.value - 1, DLogNode(projectPubKey))
    val tx2Output2 = SigmaStateBox(1, DLogNode(projectPubKey))
    val tx2 = SigmaStateTransaction(Seq(), Seq(tx2Output1, tx2Output2))

    val ctx2 = UtxoContext(currentHeight = timeout.value - 1, spendingTransaction = tx2, self = outputWithMetadata)

    //project cant' generate a proof
    val proofP2Try = projectProver.prove(crowdFundingScript, ctx2, message)
    proofP2Try.isSuccess shouldBe false

    //backer can't generate a proof
    val proofB2Try = backerProver.prove(crowdFundingScript, ctx2, message)
    proofB2Try.isSuccess shouldBe false

    //Third case: height >= timeout

    //project raised enough money but too late...
    val tx3Output1 = SigmaStateBox(minToRaise.value + 1, DLogNode(projectPubKey))
    val tx3Output2 = SigmaStateBox(1, DLogNode(projectPubKey))
    val tx3 = SigmaStateTransaction(Seq(), Seq(tx3Output1, tx3Output2))

    val ctx3 = UtxoContext(currentHeight = timeout.value, spendingTransaction = tx3, self = outputWithMetadata)

    //project cant' generate a proof
    projectProver.prove(crowdFundingScript, ctx3, message).isFailure shouldBe true

    //backer is generating a proof and it is passing verification
    val proofB = backerProver.prove(crowdFundingScript, ctx3, message).get.proof
    verifier.verify(crowdFundingScript, ctx3, proofB, message).get shouldBe true
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

    val regScript = DLogNode(userProver.dlogSecrets.head.publicImage.h)

    val script = OR(
      regScript,
      AND(
        GE(Height, Plus(SelfHeight, NonNegativeIntLeaf(demurragePeriod))),
        TxHasOutput(GE(OutputAmount, Minus(SelfAmount, NonNegativeIntLeaf(demurrageCost))), EQ(OutputScript, SelfScript))
      )
    )

    val outHeight = 100
    val outValue = 10

    val message = Blake2b256("Hello World") //normally message to be defined by spending transaction bytes

    //case 1: demurrage time hasn't come yet
    val tx1 = SigmaStateTransaction(Seq(), Seq(SigmaStateBox(outValue, script)))

    val ctx1 = UtxoContext(
      currentHeight = outHeight + demurragePeriod - 1,
      spendingTransaction = tx1,
      self = boxWithMetadata(outValue, script, outHeight))

    //user can spend all the money
    val uProof1 = userProver.prove(script, ctx1, message).get.proof
    verifier.verify(script, ctx1, uProof1, message).get shouldBe true

    //miner can't spend any money
    verifier.verify(script, ctx1, NoProof, message).get shouldBe false

    //case 2: demurrage time has come
    val ctx2 = UtxoContext(
      currentHeight = outHeight + demurragePeriod,
      spendingTransaction = tx1,
      self = boxWithMetadata(outValue, script, outHeight))

    //user can spend all the money
    val uProof2 = userProver.prove(script, ctx1, message).get.proof
    verifier.verify(script, ctx2, uProof2, message).get shouldBe true

    //miner can spend "demurrageCost" tokens
    val tx3 = SigmaStateTransaction(Seq(), Seq(SigmaStateBox(outValue - demurrageCost, script)))
    val ctx3 = UtxoContext(
      currentHeight = outHeight + demurragePeriod,
      spendingTransaction = tx3,
      self = boxWithMetadata(outValue, script, outHeight))

    verifier.verify(script, ctx3, NoProof, message).get shouldBe true

    //miner can't spend more
    val tx4 = SigmaStateTransaction(Seq(), Seq(SigmaStateBox(outValue - demurrageCost - 1, script)))
    val ctx4 = UtxoContext(
      currentHeight = outHeight + demurragePeriod,
      spendingTransaction = tx4,
      self = boxWithMetadata(outValue, script, outHeight))

    verifier.verify(script, ctx4, NoProof, message).get shouldBe false

    //miner can spend less
    val tx5 = SigmaStateTransaction(Seq(), Seq(SigmaStateBox(outValue - demurrageCost + 1, script)))
    val ctx5 = UtxoContext(
      currentHeight = outHeight + demurragePeriod,
      spendingTransaction = tx5,
      self = boxWithMetadata(outValue, script, outHeight))

    verifier.verify(script, ctx5, NoProof, message).get shouldBe true
  }

  /**
    * The script is asking for a hash function preimage. The "proof" could be replayed, so not really a proof.
    */
  property("prover enriching context") {
    val prover = new UtxoProvingInterpreter
    val preimage = prover.contextExtenders.head._2.value
    val prop = EQ(CalcBlake2b256(CustomByteArray(Helpers.tagInt(preimage))), ByteArrayLeaf(Blake2b256(preimage)))

    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 0, spendingTransaction = null, self = boxWithMetadata(0, TrueLeaf))
    val pr = prover.prove(prop, ctx, message).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new UtxoInterpreter
    verifier.verify(prop, ctx, pr.proof, message).get shouldBe false //context w/out extensions
    verifier.verify(prop, ctxv, pr.proof, message).get shouldBe true
  }

  property("prover enriching context 2") {
    val prover = new UtxoProvingInterpreter
    val preimage1 = prover.contextExtenders.head._2.value
    val preimage2 = prover.contextExtenders.tail.head._2.value
    val prop = EQ(CalcBlake2b256(Append(CustomByteArray(Helpers.tagInt(preimage2)),
      CustomByteArray(Helpers.tagInt(preimage1)))
    ), ByteArrayLeaf(Blake2b256(preimage2 ++ preimage1)))

    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 0, spendingTransaction = null, self = boxWithMetadata(0, TrueLeaf))
    val pr = prover.prove(prop, ctx, message).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new UtxoInterpreter
    verifier.verify(prop, ctx, pr.proof, message).get shouldBe false //context w/out extensions
    verifier.verify(prop, ctxv, pr.proof, message).get shouldBe true
  }

  property("prover enriching context - xor") {
    val v1 = Base16.decode("abcdef7865")
    val k1 = Helpers.tagInt(v1)

    val v2 = Base16.decode("10abdca345")
    val k2 = Helpers.tagInt(v2)

    val r = Base16.decode("bb6633db20")

    val prover = new UtxoProvingInterpreter()
      .withContextExtender(k1, ByteArrayLeaf(v1))
      .withContextExtender(k2, ByteArrayLeaf(v2))

    val prop = EQ(Xor(CustomByteArray(k1), CustomByteArray(k2)), ByteArrayLeaf(r))

    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 0, spendingTransaction = null, self = boxWithMetadata(0, TrueLeaf))
    val pr = prover.prove(prop, ctx, message).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new UtxoInterpreter
    verifier.verify(prop, ctx, pr.proof, message).get shouldBe false //context w/out extensions
    verifier.verify(prop, ctxv, pr.proof, message).get shouldBe true
  }

  property("context enriching mixed w. crypto") {
    val prover = new UtxoProvingInterpreter
    val preimage = prover.contextExtenders.head._2.value
    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = AND(
      pubkey,
      EQ(CalcBlake2b256(CustomByteArray(Helpers.tagInt(preimage))), ByteArrayLeaf(Blake2b256(preimage)))
    )

    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 0, spendingTransaction = null, self = boxWithMetadata(0, TrueLeaf))
    val pr = prover.prove(prop, ctx, message).get

    val ctxv = ctx.withExtension(pr.extension)

    pr.proof.isInstanceOf[SchnorrNode] shouldBe true

    val verifier = new UtxoInterpreter
    verifier.verify(prop, ctx, pr.proof, message).getOrElse(false) shouldBe false //context w/out extensions
    verifier.verify(prop, ctxv, pr.proof, message).get shouldBe true
  }

  property("context enriching mixed w. crypto 2") {
    val prover = new UtxoProvingInterpreter
    val preimage1 = prover.contextExtenders.head._2.value
    val preimage2 = prover.contextExtenders.tail.head._2.value
    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = AND(
      pubkey,
      EQ(
        CalcBlake2b256(Append(CustomByteArray(Helpers.tagInt(preimage1)), CustomByteArray(Helpers.tagInt(preimage2)))),
        ByteArrayLeaf(Blake2b256(preimage1 ++ preimage2))
      )
    )

    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 0, spendingTransaction = null, self = boxWithMetadata(0, TrueLeaf))
    val pr = prover.prove(prop, ctx, message).get

    val ctxv = ctx.withExtension(pr.extension)

    pr.proof.isInstanceOf[SchnorrNode] shouldBe true

    val verifier = new UtxoInterpreter
    verifier.verify(prop, ctx, pr.proof, message).getOrElse(false) shouldBe false //context w/out extensions
    verifier.verify(prop, ctxv, pr.proof, message).get shouldBe true
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

    val x = proverA.contextExtenders.head._2.value
    val hx = ByteArrayLeaf(Blake2b256(x))

    val height1 = 100000
    val height2 = 50000

    val deadlineA = 1000
    val deadlineB = 500

    //chain1 script
    val prop1 = OR(
      AND(GT(Height, NonNegativeIntLeaf(height1 + deadlineA)), pubkeyA),
      AND(pubkeyB, EQ(CalcBlake2b256(CustomByteArray(Helpers.tagInt(x))), hx))
    )

    //chain2 script
    val prop2 = OR(
      AND(GT(Height, NonNegativeIntLeaf(height2 + deadlineB)), pubkeyB),
      AND(pubkeyA, EQ(CalcBlake2b256(CustomByteArray(Helpers.tagInt(x))), hx))
    )

    //fake message, in a real-life a message is to be derived from a spending transaction
    val message = Blake2b256("Hello World")

    //Preliminary checks:

    //B can't spend coins of A in chain1 (generate a valid proof)
    val ctxf1 = UtxoContext(currentHeight = height1 + 1, spendingTransaction = null, self = fakeSelf)
    proverB.prove(prop1, ctxf1, message).isSuccess shouldBe false

    //A can't withdraw her coins in chain1 (generate a valid proof)
    println(proverA.prove(prop1, ctxf1, message))
    proverA.prove(prop1, ctxf1, message).isFailure shouldBe true

    //B cant't withdraw his coins in chain2 (generate a valid proof)
    val ctxf2 = UtxoContext(currentHeight = height2 + 1, spendingTransaction = null, self = fakeSelf)
    proverB.prove(prop2, ctxf2, message).isSuccess shouldBe false

    //Successful run below:

    //A spends coins of B in chain2
    val ctx1 = UtxoContext(currentHeight = height2 + 1, spendingTransaction = null, self = fakeSelf)
    val pr = proverA.prove(prop2, ctx1, message).get
    verifier.verify(prop2, ctx1, pr, message).get shouldBe true

    //B extracts preimage x of hx
    val t = pr.extension.values.head
    val (tx, bx) = (t._1, t._2)
    val proverB2 = proverB.withContextExtender(tx, bx.asInstanceOf[ByteArrayLeaf])

    //B spends coins of A in chain1 with knowledge of x
    val ctx2 = UtxoContext(currentHeight = height1 + 1, spendingTransaction = null, self = fakeSelf)
    val pr2 = proverB2.prove(prop1, ctx2, message).get
    verifier.verify(prop1, ctx2, pr2, message).get shouldBe true
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

    //fake message, in a real-life a message is to be derived from a spending transaction
    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 1, spendingTransaction = null, self = fakeSelf)

    val prA = proverA.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, prA, message).get shouldBe true

    val prB = proverB.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, prB, message).get shouldBe true

    proverC.prove(prop, ctx, message).isFailure shouldBe true
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

    //fake message, in a real-life a message is to be derived from a spending transaction
    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 1, spendingTransaction = null, self = fakeSelf)

    val prA = proverA.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, prA, message).get shouldBe true

    val prB = proverB.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, prB, message).get shouldBe true

    val prC = proverC.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, prC, message).get shouldBe true
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

    //fake message, in a real-life a message is to be derived from a spending transaction
    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 1, spendingTransaction = null, self = fakeSelf)

    val prA = proverA.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, prA, message).get shouldBe true
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

    //fake message, in a real-life a message is to be derived from a spending transaction
    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 1, spendingTransaction = null, self = fakeSelf)

    proverA.prove(prop, ctx, message).isFailure shouldBe true
    proverB.prove(prop, ctx, message).isFailure shouldBe true
    proverC.prove(prop, ctx, message).isFailure shouldBe true
    proverD.prove(prop, ctx, message).isFailure shouldBe true

    val proverAB = proverA.withSecrets(Seq(proverB.dlogSecrets.head))
    val pr = proverAB.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, pr, message).get shouldBe true

    val proverCD = proverC.withSecrets(Seq(proverD.dlogSecrets.head))
    val pr2 = proverCD.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, pr2, message).get shouldBe true
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

    //fake message, in a real-life a message is to be derived from a spending transaction
    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 1, spendingTransaction = null, self = fakeSelf)

    proverA.prove(prop, ctx, message).isFailure shouldBe true
    proverB.prove(prop, ctx, message).isFailure shouldBe true

    val prC = proverC.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, prC, message).get shouldBe true

    val prD = proverD.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, prD, message).get shouldBe true

    val proverAB = proverA.withSecrets(Seq(proverB.dlogSecrets.head))
    val pr = proverAB.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, pr, message).get shouldBe true
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

    //fake message, in a real-life a message is to be derived from a spending transaction
    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 1, spendingTransaction = null, self = fakeSelf)

    proverA.prove(prop, ctx, message).isFailure shouldBe true
    proverB.prove(prop, ctx, message).isFailure shouldBe true
    proverC.prove(prop, ctx, message).isFailure shouldBe true
    proverD.prove(prop, ctx, message).isFailure shouldBe true

    val proverAC = proverA.withSecrets(Seq(proverC.dlogSecrets.head))
    val pr = proverAC.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, pr, message).get shouldBe true

    val proverBD = proverB.withSecrets(Seq(proverD.dlogSecrets.head))
    val pr2 = proverBD.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, pr2, message).get shouldBe true
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

    //fake message, in a real-life a message is to be derived from a spending transaction
    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 1, spendingTransaction = null, self = fakeSelf)

    proverA.prove(prop, ctx, message).isFailure shouldBe true
    proverB.prove(prop, ctx, message).isFailure shouldBe true
    proverC.prove(prop, ctx, message).isFailure shouldBe true
    proverD.prove(prop, ctx, message).isFailure shouldBe true

    val proverAB = proverA.withSecrets(Seq(proverB.dlogSecrets.head))
    proverAB.prove(prop, ctx, message).isFailure shouldBe true

    val proverABC = proverAB.withSecrets(Seq(proverC.dlogSecrets.head))
    val prABC = proverABC.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, prABC, message).get shouldBe true

    val proverABD = proverAB.withSecrets(Seq(proverC.dlogSecrets.head))
    val prABD = proverABD.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, prABD, message).get shouldBe true
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

    //fake message, in a real-life a message is to be derived from a spending transaction
    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 1, spendingTransaction = null, self = fakeSelf)

    val prA = proverA.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, prA, message).get shouldBe true

    val prB = proverB.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, prB, message).get shouldBe true

    val prC = proverC.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, prC, message).get shouldBe true

    val prD = proverD.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, prD, message).get shouldBe true
  }

  property("complex sig scheme - OR w. predicate") {
    val proverA = new UtxoProvingInterpreter
    val proverB = new UtxoProvingInterpreter
    val proverC = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage

    val prop = OR(pubkeyA, pubkeyB, GT(Height, NonNegativeIntLeaf(500)))

    //fake message, in a real-life a message is to be derived from a spending transaction
    val message = Blake2b256("Hello World")

    val ctx1 = UtxoContext(currentHeight = 1, spendingTransaction = null, self = fakeSelf)
    val prA = proverA.prove(prop, ctx1, message).get
    verifier.verify(prop, ctx1, prA, message).get shouldBe true
    val prB = proverB.prove(prop, ctx1, message).get
    verifier.verify(prop, ctx1, prB, message).get shouldBe true
    proverC.prove(prop, ctx1, message).isFailure shouldBe true

    val ctx2 = UtxoContext(currentHeight = 501, spendingTransaction = null, self = fakeSelf)
    val prC = proverC.prove(prop, ctx2, message).get
    verifier.verify(prop, ctx2, prC, message).get shouldBe true
  }

  property("complex sig scheme - OR of OR and AND w. predicate") {
    val proverA = new UtxoProvingInterpreter
    val proverB = new UtxoProvingInterpreter
    val proverC = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyC = proverC.dlogSecrets.head.publicImage

    val prop = OR(OR(pubkeyA, pubkeyB), AND(pubkeyC, GT(Height, NonNegativeIntLeaf(500))))

    //fake message, in a real-life a message is to be derived from a spending transaction
    val message = Blake2b256("Hello World")
    val ctx1 = UtxoContext(currentHeight = 1, spendingTransaction = null, self = fakeSelf)

    val prA = proverA.prove(prop, ctx1, message).get
    verifier.verify(prop, ctx1, prA, message).get shouldBe true
    val prB = proverB.prove(prop, ctx1, message).get
    verifier.verify(prop, ctx1, prB, message).get shouldBe true
    proverC.prove(prop, ctx1, message).isFailure shouldBe true


    val ctx2 = UtxoContext(currentHeight = 501, spendingTransaction = null, self = fakeSelf)

    val prA2 = proverA.prove(prop, ctx2, message).get
    verifier.verify(prop, ctx2, prA2, message).get shouldBe true
    val prB2 = proverB.prove(prop, ctx2, message).get
    verifier.verify(prop, ctx2, prB2, message).get shouldBe true
    val prC2 = proverC.prove(prop, ctx2, message).get
    verifier.verify(prop, ctx2, prC2, message).get shouldBe true
  }

  property("DH tuple"){
    val prover = new UtxoProvingInterpreter
    val fakeProver = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val secret = prover.dhSecrets.head

    val ci = secret.commonInput

    val prop = DiffieHellmanTupleNode(ci.g, ci.h, ci.u, ci.v)
    val wrongProp = DiffieHellmanTupleNode(ci.g, ci.h, ci.u, ci.u)

    //fake message, in a real-life a message is to be derived from a spending transaction
    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 1, spendingTransaction = null, self = fakeSelf)

    val pr = prover.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, pr, message).get shouldBe true

    fakeProver.prove(prop, ctx, message).isSuccess shouldBe false
    prover.prove(wrongProp, ctx, message).isSuccess shouldBe false
  }

  property("DH tuple - simulation"){
    val proverA = new UtxoProvingInterpreter
    val proverB = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubdhB = proverB.dhSecrets.head.publicImage

    val prop = OR(pubkeyA, pubdhB)

    //fake message, in a real-life a message is to be derived from a spending transaction
    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 1, spendingTransaction = null, self = fakeSelf)

    val prA = proverA.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, prA, message).get shouldBe true
  }

  property("DH tuple and DLOG"){
    val proverA = new UtxoProvingInterpreter
    val proverB = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubdhA = proverA.dhSecrets.head.publicImage

    val prop = AND(pubkeyA, pubdhA)

    //fake message, in a real-life a message is to be derived from a spending transaction
    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 1, spendingTransaction = null, self = fakeSelf)

    val prA = proverA.prove(prop, ctx, message).get
    verifier.verify(prop, ctx, prA, message).get shouldBe true

    proverB.prove(prop, ctx, message).isSuccess shouldBe false
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

    val newBoxes = Seq(newBox1, newBox2)

    val properBytes = Bytes.concat(newBoxes.map(_.bytes):_*)

    val properHash = Blake2b256(properBytes)

    val spendingTransaction = SigmaStateTransaction(Seq(), newBoxes)

    def mixingRequestProp(sender: DLogNode, timeout: Int) = OR(
      AND(LE(Height, NonNegativeIntLeaf(timeout)), EQ(CalcBlake2b256(TxOutBytes), ByteArrayLeaf(properHash))),
      AND(GT(Height, NonNegativeIntLeaf(timeout)), sender)
    )

    //fake message, in a real-life a message is to be derived from a spending transaction
    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 50, spendingTransaction, self = fakeSelf)

    //before timeout
    val prA = proverA.prove(mixingRequestProp(pubkeyA, 100), ctx, message).get
    verifier.verify(mixingRequestProp(pubkeyA, 100), ctx, prA, message).get shouldBe true
    verifier.verify(mixingRequestProp(pubkeyB, 100), ctx, prA, message).get shouldBe true

    //after timeout
    val prA2 = proverA.prove(mixingRequestProp(pubkeyA, 40), ctx, message).get
    println(prA2)
    verifier.verify(mixingRequestProp(pubkeyA, 40), ctx, prA2, message).get shouldBe true
    verifier.verify(mixingRequestProp(pubkeyB, 40), ctx, prA2, message).isSuccess shouldBe false
  }
}