package sigmastate.utxo

import com.google.common.primitives.Bytes
import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.{PropertyChecks, GeneratorDrivenPropertyChecks}
import scapi.sigma.DLogProtocol.ProveDlog
import scapi.sigma.ProveDiffieHellmanTuple
import scorex.crypto.encode.Base16
import scorex.crypto.hash.{Digest32, Blake2b256, Blake2b256Unsafe}
import sigmastate._
import sigmastate.Values._
import BoxHelpers.createBox
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Lookup}
import sigmastate.interpreter.GroupSettings
import sigmastate.lang.{SigmaBinder, SigmaParser, SigmaTyper, SigmaSpecializer}
import sigmastate.lang.Terms._
import sigmastate.utxo.ErgoBox._


class UtxoInterpreterSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  implicit def grElemConvert(leafConstant: GroupElementConstant): GroupSettings.EcPointType = leafConstant.value
  implicit def grLeafConvert(elem: GroupSettings.EcPointType): Value[SGroupElement.type] = GroupElementConstant(elem)

  import BoxHelpers.{fakeMessage, fakeSelf}

  def parse(x: String): SValue = SigmaParser(x).get.value

  def compile(env: Map[String, Any], code: String): Value[SType] = {
    val parsed = parse(code)
    val binder = new SigmaBinder(env)
    val bound = binder.bind(parsed)
    val st = new SigmaTree(bound)
    val typer = new SigmaTyper(env, st)
    val typed = typer.typecheck(bound)
    val spec = new SigmaSpecializer
    val ir = spec.specialize(typed)
    ir
  }

  property("scripts EQ/NEQ") {
    val prover1 = new UtxoProvingInterpreter
    val prover2 = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val h1 = prover1.dlogSecrets.head.publicImage
    val h2 = prover2.dlogSecrets.head.publicImage

    val ctx = UtxoContext.dummy(fakeSelf)

    val e = compile(Map("h1" -> h1.bytes, "h2" -> h2.bytes), "h1 == h1")
    val exp = EQ(ByteArrayConstant(h1.bytes), ByteArrayConstant(h1.bytes))
    e shouldBe exp

    verifier.reduceToCrypto(exp, ctx)
      .get.isInstanceOf[TrueLeaf.type] shouldBe true

    verifier.reduceToCrypto(EQ(ByteArrayConstant(h1.bytes), ByteArrayConstant(h2.bytes)), ctx)
      .get.isInstanceOf[FalseLeaf.type] shouldBe true
  }

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

    val timeout = IntConstant(100)
    val minToRaise = IntConstant(1000)

    val env = Map(
      "timeout" -> 100,
      "minToRaise" -> 1000,
      "backerPubKey" -> backerPubKey,
      "projectPubKey" -> projectPubKey,
    )
    val compiledScript = compile(env,
      """{
       | let c1 = HEIGHT >= timeout && backerPubKey
       | let c2 = allOf(Array(
       |   HEIGHT < timeout,
       |   projectPubKey,
       |   OUTPUTS.exists(fun (out: Box) = {
       |     out.value >= minToRaise && out.propositionBytes == projectPubKey.propBytes
       |   })
       | ))
       | c1 || c2
       | }
      """.stripMargin)

//    // (height >= timeout /\ dlog_g backerKey) \/ (height < timeout /\ dlog_g projKey /\ has_output(amount >= minToRaise, proposition = dlog_g projKey)
    val crowdFundingScript = OR(
      AND(GE(Height, timeout), backerPubKey),
      AND(
        Seq(
          LT(Height, timeout),
          projectPubKey,
          Exists(Outputs, 21,
            AND(
              GE(ExtractAmount(TaggedBox(21)), minToRaise),
              EQ(ExtractScriptBytes(TaggedBox(21)), ByteArrayConstant(projectPubKey.propBytes))
            )
          )
        )
      )
    )
    compiledScript shouldBe crowdFundingScript

    val outputToSpend = ErgoBox(10, crowdFundingScript)

    //First case: height < timeout, project is able to claim amount of tokens not less than required threshold

    val tx1Output1 = ErgoBox(minToRaise.value, projectPubKey)
    val tx1Output2 = ErgoBox(1, projectPubKey)

    //normally this transaction would invalid, but we're not checking it in this test
    val tx1 = ErgoTransaction(IndexedSeq(), IndexedSeq(tx1Output1, tx1Output2))

    val ctx1 = UtxoContext(
      currentHeight = timeout.value - 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx1,
      self = outputToSpend)

    //project is generating a proof and it is passing verification
    val proofP = projectProver.prove(crowdFundingScript, ctx1, fakeMessage).get.proof
    verifier.verify(crowdFundingScript, ctx1, proofP, fakeMessage).get shouldBe true

    //backer can't generate a proof
    backerProver.prove(crowdFundingScript, ctx1, fakeMessage).isFailure shouldBe true


    //Second case: height < timeout, project is NOT able to claim amount of tokens not less than required threshold

    val tx2Output1 = ErgoBox(minToRaise.value - 1, projectPubKey)
    val tx2Output2 = ErgoBox(1, projectPubKey)
    val tx2 = ErgoTransaction(IndexedSeq(), IndexedSeq(tx2Output1, tx2Output2))

    val ctx2 = UtxoContext(
      currentHeight = timeout.value - 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx2,
      self = outputToSpend)

    //project cant' generate a proof
    val proofP2Try = projectProver.prove(crowdFundingScript, ctx2, fakeMessage)
    proofP2Try.isSuccess shouldBe false

    //backer can't generate a proof
    val proofB2Try = backerProver.prove(crowdFundingScript, ctx2, fakeMessage)
    proofB2Try.isSuccess shouldBe false

    //Third case: height >= timeout

    //project raised enough money but too late...
    val tx3Output1 = ErgoBox(minToRaise.value + 1, projectPubKey)
    val tx3Output2 = ErgoBox(1, projectPubKey)
    val tx3 = ErgoTransaction(IndexedSeq(), IndexedSeq(tx3Output1, tx3Output2))

    val ctx3 = UtxoContext(
      currentHeight = timeout.value,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx3,
      self = outputToSpend)

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
    * We assume that it is enforced by a consensus protocol to store height when an input got into a block in the
    * register R3 (if the transaction is not included into the blockchain yet, then R3 contains the current height of
    * the blockchain).
    *
    * (regular_script) ∨
    * (height > (self.R3 + demurrage_period ) ∧ has_output(value >= self.value − demurrage_cost, script = self.script))
    */
  property("Evaluation - Demurrage Example") {
    val demurragePeriod = 100
    val demurrageCost = 2

    //a blockchain node veryfing a block containing a spending transaction
    val verifier = new UtxoInterpreter

    //backer's prover with his private key
    val userProver = new UtxoProvingInterpreter

    val regScript = userProver.dlogSecrets.head.publicImage

//    val env = Map(
//      "demurragePeriod" -> demurragePeriod,
//      "demurrageCost" -> demurrageCost,
//      "regScript" -> regScript,
//    )
//    val compiledScript = compile(env,
//      """{
//       | let c2 = allOf(Array(
//       |   HEIGHT >= SELF.R3 + demurragePeriod,
//       |   OUTPUTS.exists(fun (out: Box) = {
//       |     out.value >= SELF.value - demurrageCost && out.propositionBytes == SELF.propositionBytes
//       |   })
//       | ))
//       | regScript || c2
//       | }
//      """.stripMargin)
    val script = OR(
      regScript,
      AND(
        GE(Height, Plus(ExtractRegisterAs[SInt.type](Self, R3), IntConstant(demurragePeriod))),
        Exists(Outputs, 21,
          AND(
            GE(ExtractAmount(TaggedBox(21)), Minus(ExtractAmount(Self), IntConstant(demurrageCost))),
            EQ(ExtractScriptBytes(TaggedBox(21)), ExtractScriptBytes(Self))
          )
        )
      )
    )
//    compiledScript shouldBe script

    val outHeight = 100
    val outValue = 10
    val curHeight = outHeight + demurragePeriod

    //case 1: demurrage time hasn't come yet
    val tx1 = ErgoTransaction(
      IndexedSeq(),
      IndexedSeq(ErgoBox(outValue, script, additionalRegisters = Map(R3 -> IntConstant(curHeight)))))

    val ctx1 = UtxoContext(
      currentHeight = outHeight + demurragePeriod - 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx1,
      self = createBox(outValue, script, additionalRegisters = Map(R3 -> IntConstant(outHeight))))

    //user can spend all the money
    val uProof1 = userProver.prove(script, ctx1, fakeMessage).get.proof
    verifier.verify(script, ctx1, uProof1, fakeMessage).get shouldBe true

    //miner can't spend any money
    verifier.verify(script, ctx1, NoProof, fakeMessage).get shouldBe false

    //case 2: demurrage time has come
    val ctx2 = UtxoContext(
      currentHeight = outHeight + demurragePeriod,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx1,
      self = createBox(outValue, script, additionalRegisters = Map(R3 -> IntConstant(outHeight))))

    //user can spend all the money
    val uProof2 = userProver.prove(script, ctx1, fakeMessage).get.proof
    verifier.verify(script, ctx2, uProof2, fakeMessage).get shouldBe true

    //miner can spend "demurrageCost" tokens
    val tx3 = ErgoTransaction(IndexedSeq(),
      IndexedSeq(ErgoBox(outValue - demurrageCost, script, additionalRegisters = Map(R3 -> IntConstant(curHeight)))))
    val ctx3 = UtxoContext(
      currentHeight = outHeight + demurragePeriod,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx3,
      self = createBox(outValue, script, additionalRegisters = Map(R3 -> IntConstant(outHeight))))


    assert(ctx3.spendingTransaction.outputs.head.propositionBytes sameElements ctx3.self.propositionBytes)

    verifier.verify(script, ctx3, NoProof, fakeMessage).get shouldBe true

    //miner can't spend more
    val tx4 = ErgoTransaction(IndexedSeq(),
      IndexedSeq(ErgoBox(outValue - demurrageCost - 1, script, additionalRegisters = Map(R3 -> IntConstant(curHeight)))))
    val ctx4 = UtxoContext(
      currentHeight = outHeight + demurragePeriod,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx4,
      self = createBox(outValue, script, additionalRegisters = Map(R3 -> IntConstant(outHeight))))

    verifier.verify(script, ctx4, NoProof, fakeMessage).get shouldBe false

    //miner can spend less
    val tx5 = ErgoTransaction(IndexedSeq(),
      IndexedSeq(ErgoBox(outValue - demurrageCost + 1, script, additionalRegisters = Map(R3 -> IntConstant(curHeight)))))

    val ctx5 = UtxoContext(
      currentHeight = outHeight + demurragePeriod,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx5,
      self = createBox(outValue, script, additionalRegisters = Map(R3 -> IntConstant(outHeight))))

    verifier.verify(script, ctx5, NoProof, fakeMessage).get shouldBe true
  }

  /**
    * The script is asking for a hash function preimage. The "proof" could be replayed, so not really a proof.
    */
  property("prover enriching context") {
    val prover = new UtxoProvingInterpreter
    val preimage = prover.contextExtenders(1: Byte).value.asInstanceOf[Array[Byte]]

    val env = Map("blake" -> Blake2b256(preimage))
    val compiledScript = compile(env,
      """{
       |  blake2b256(taggedByteArray(1)) == blake
       |}
      """.stripMargin)

    val prop = EQ(CalcBlake2b256(TaggedByteArray(1)), ByteArrayConstant(Blake2b256(preimage)))
    compiledScript shouldBe prop

    val ctx = UtxoContext.dummy(fakeSelf)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new UtxoInterpreter
    verifier.verify(prop, ctx, pr.proof, fakeMessage).get shouldBe false //context w/out extensions
    verifier.verify(prop, ctxv, pr.proof, fakeMessage).get shouldBe true
  }

  property("prover enriching context 2") {
    val prover = new UtxoProvingInterpreter
    val preimage1 = prover.contextExtenders(1).value.asInstanceOf[Array[Byte]]
    val preimage2 = prover.contextExtenders(2).value.asInstanceOf[Array[Byte]]

    val env = Map("blake" -> Blake2b256(preimage2 ++ preimage1))
    val compiledScript = compile(env,
      """{
       |  blake2b256(taggedByteArray(2) ++ taggedByteArray(1)) == blake
       |}
      """.stripMargin)

    val prop = EQ(CalcBlake2b256(AppendBytes(TaggedByteArray(2), TaggedByteArray(1))),
      ByteArrayConstant(Blake2b256(preimage2 ++ preimage1)))
    compiledScript shouldBe prop

    val ctx = UtxoContext.dummy(fakeSelf)
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
      .withContextExtender(k1, ByteArrayConstant(v1))
      .withContextExtender(k2, ByteArrayConstant(v2))

    val env = Map("k1" -> k1.toInt, "k2" -> k2.toInt, "r" -> r)
    val compiledScript = compile(env,
      """{
       |  (taggedByteArray(k1) | taggedByteArray(k2)) == r
       |}
      """.stripMargin)

    val prop = EQ(Xor(TaggedByteArray(k1), TaggedByteArray(k2)), ByteArrayConstant(r))
    compiledScript shouldBe prop

    val ctx = UtxoContext.dummy(fakeSelf)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new UtxoInterpreter
    verifier.verify(prop, ctx, pr.proof, fakeMessage).get shouldBe false //context w/out extensions
    verifier.verify(prop, ctxv, pr.proof, fakeMessage).get shouldBe true
  }

  property("context enriching mixed w. crypto") {
    val prover = new UtxoProvingInterpreter
    val preimage = prover.contextExtenders(1).value.asInstanceOf[Array[Byte]]
    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("blake" -> Blake2b256(preimage), "pubkey" -> pubkey)
    val compiledScript = compile(env,
      """{
       |  pubkey && blake2b256(taggedByteArray(1)) == blake
       |}
      """.stripMargin)
    val prop = AND(
      pubkey,
      EQ(CalcBlake2b256(TaggedByteArray(1)), ByteArrayConstant(Blake2b256(preimage)))
    )
    compiledScript shouldBe prop

    val ctx = UtxoContext.dummy(fakeSelf)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    pr.proof.isInstanceOf[SchnorrNode] shouldBe true

    val verifier = new UtxoInterpreter
    verifier.verify(prop, ctx, pr.proof, fakeMessage).getOrElse(false) shouldBe false //context w/out extensions
    verifier.verify(prop, ctxv, pr.proof, fakeMessage).get shouldBe true
  }

  property("context enriching mixed w. crypto 2") {
    val prover = new UtxoProvingInterpreter
    val preimage1 = prover.contextExtenders(1).value.asInstanceOf[Array[Byte]]
    val preimage2 = prover.contextExtenders(2).value.asInstanceOf[Array[Byte]]
    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("blake" -> Blake2b256(preimage1 ++ preimage2), "pubkey" -> pubkey)
    val compiledScript = compile(env,
      """{
       |  pubkey && blake2b256(taggedByteArray(1) ++ taggedByteArray(2)) == blake
       |}
      """.stripMargin)

    val prop = AND(
      pubkey,
      EQ(
        CalcBlake2b256(AppendBytes(TaggedByteArray(1), TaggedByteArray(2))),
        ByteArrayConstant(Blake2b256(preimage1 ++ preimage2))
      )
    )
    compiledScript shouldBe prop

    val ctx = UtxoContext.dummy(fakeSelf)
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
    val compiledProp1 = compile(env,
      """{
       |  anyOf(Array(
       |    HEIGHT > height1 + deadlineA && pubkeyA,
       |    pubkeyB && blake2b256(taggedByteArray(1)) == hx
       |  ))
       |}
      """.stripMargin)

    //chain1 script
    val prop1 = OR(
      AND(GT(Height, Plus(IntConstant(height1), IntConstant(deadlineA))), pubkeyA),
      AND(pubkeyB, EQ(CalcBlake2b256(TaggedByteArray(1)), hx))
    )
    compiledProp1 shouldBe prop1

    val compiledProp2 = compile(env,
      """{
       |  anyOf(Array(
       |    HEIGHT > height2 + deadlineB && pubkeyB,
       |    pubkeyA && blake2b256(taggedByteArray(1)) == hx
       |  ))
       |}
      """.stripMargin)

    //chain2 script
    val prop2 = OR(
      AND(GT(Height, Plus(IntConstant(height2), IntConstant(deadlineB))), pubkeyB),
      AND(pubkeyA, EQ(CalcBlake2b256(TaggedByteArray(1)), hx))
    )
    compiledProp2 shouldBe prop2

    //Preliminary checks:

    //B can't spend coins of A in chain1 (generate a valid proof)
    val ctxf1 = UtxoContext(
      currentHeight = height1 + 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)
    proverB.prove(prop1, ctxf1, fakeMessage).isSuccess shouldBe false

    //A can't withdraw her coins in chain1 (generate a valid proof)
    println(proverA.prove(prop1, ctxf1, fakeMessage))
    proverA.prove(prop1, ctxf1, fakeMessage).isFailure shouldBe true

    //B cant't withdraw his coins in chain2 (generate a valid proof)
    val ctxf2 = UtxoContext(
      currentHeight = height2 + 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(), spendingTransaction = null, self = fakeSelf)
    proverB.prove(prop2, ctxf2, fakeMessage).isSuccess shouldBe false

    //Successful run below:

    //A spends coins of B in chain2
    val ctx1 = UtxoContext(
      currentHeight = height2 + 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)
    val pr = proverA.prove(prop2, ctx1, fakeMessage).get
    verifier.verify(prop2, ctx1, pr, fakeMessage).get shouldBe true

    //B extracts preimage x of hx
    val t = pr.extension.values(1)
    val proverB2 = proverB.withContextExtender(1, t.asInstanceOf[ByteArrayConstant])

    //B spends coins of A in chain1 with knowledge of x
    val ctx2 = UtxoContext(
      currentHeight = height1 + 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)
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

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB)
    val compiledProp = compile(env, """pubkeyA || pubkeyB""")

    val prop = OR(pubkeyA, pubkeyB)
    compiledProp shouldBe prop

    val ctx = UtxoContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

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

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC)
    val compiledProp = compile(env, """anyOf(Array(pubkeyA, pubkeyB, pubkeyC))""")

    val prop = OR(pubkeyA, pubkeyB, pubkeyC)
    compiledProp shouldBe prop

    val ctx = UtxoContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

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

    val env = Map("pubkeyA1" -> pubkeyA1, "pubkeyA2" -> pubkeyA2, "pubkeyA3" -> pubkeyA3, "pubkeyA4" -> pubkeyA4)
    val compiledProp = compile(env, """anyOf(Array(pubkeyA1, pubkeyA2, pubkeyA3, pubkeyA4))""")

    val prop = OR(Seq(pubkeyA1, pubkeyA2, pubkeyA3, pubkeyA4))
    compiledProp shouldBe prop

    val ctx = UtxoContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

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

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    val compiledProp = compile(env, """pubkeyA && pubkeyB || pubkeyC && pubkeyD""")

    val prop = OR(AND(pubkeyA, pubkeyB), AND(pubkeyC, pubkeyD))
    compiledProp shouldBe prop

    val ctx = UtxoContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

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

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    val compiledProp = compile(env, """pubkeyA && pubkeyB || (pubkeyC || pubkeyD)""")

    val prop = OR(AND(pubkeyA, pubkeyB), OR(pubkeyC, pubkeyD))
    compiledProp shouldBe prop

    val ctx = UtxoContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

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

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    val compiledProp = compile(env, """(pubkeyA || pubkeyB) && (pubkeyC || pubkeyD)""")

    val prop = AND(OR(pubkeyA, pubkeyB), OR(pubkeyC, pubkeyD))
    compiledProp shouldBe prop

    val ctx = UtxoContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

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

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    val compiledProp = compile(env, """(pubkeyA && pubkeyB) && (pubkeyC || pubkeyD)""")

    val prop = AND(AND(pubkeyA, pubkeyB), OR(pubkeyC, pubkeyD))
    compiledProp shouldBe prop

    val ctx = UtxoContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

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

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC, "pubkeyD" -> pubkeyD)
    val compiledProp = compile(env, """(pubkeyA || pubkeyB) || (pubkeyC || pubkeyD)""")

    val prop = OR(OR(pubkeyA, pubkeyB), OR(pubkeyC, pubkeyD))
    compiledProp shouldBe prop
    
    val ctx = UtxoContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

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

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB)
    val compiledProp = compile(env, """anyOf(Array(pubkeyA, pubkeyB, HEIGHT > 500))""")

    val prop = OR(pubkeyA, pubkeyB, GT(Height, IntConstant(500)))
    compiledProp shouldBe prop

    val ctx1 = UtxoContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)
    val prA = proverA.prove(prop, ctx1, fakeMessage).get
    verifier.verify(prop, ctx1, prA, fakeMessage).get shouldBe true
    val prB = proverB.prove(prop, ctx1, fakeMessage).get
    verifier.verify(prop, ctx1, prB, fakeMessage).get shouldBe true
    proverC.prove(prop, ctx1, fakeMessage).isFailure shouldBe true

    val ctx2 = UtxoContext(
      currentHeight = 501,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)
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

    val env = Map("pubkeyA" -> pubkeyA, "pubkeyB" -> pubkeyB, "pubkeyC" -> pubkeyC)
    val compiledProp = compile(env, """anyOf(Array(pubkeyA || pubkeyB, pubkeyC && HEIGHT > 500))""")

    val prop = OR(OR(pubkeyA, pubkeyB), AND(pubkeyC, GT(Height, IntConstant(500))))
    compiledProp shouldBe prop
    
    val ctx1 = UtxoContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    val prA = proverA.prove(prop, ctx1, fakeMessage).get
    verifier.verify(prop, ctx1, prA, fakeMessage).get shouldBe true
    val prB = proverB.prove(prop, ctx1, fakeMessage).get
    verifier.verify(prop, ctx1, prB, fakeMessage).get shouldBe true
    proverC.prove(prop, ctx1, fakeMessage).isFailure shouldBe true


    val ctx2 = UtxoContext(
      currentHeight = 501,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

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

    val ctx = UtxoContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

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

    val env = Map("pubkeyA" -> pubkeyA, "pubdhB" -> pubdhB)
    val compiledProp = compile(env, """pubkeyA || pubdhB""")

    val prop = OR(pubkeyA, pubdhB)
    compiledProp shouldBe prop
    
    val ctx = UtxoContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    val prA = proverA.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prA, fakeMessage).get shouldBe true
  }

  property("DH tuple and DLOG") {
    val proverA = new UtxoProvingInterpreter
    val proverB = new UtxoProvingInterpreter

    val verifier = new UtxoInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubdhA = proverA.dhSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubdhA" -> pubdhA)
    val compiledProp = compile(env, """pubkeyA && pubdhA""")

    val prop = AND(pubkeyA, pubdhA)
    compiledProp shouldBe prop
    
    val ctx = UtxoContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

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

    val newBox1 = ErgoBox(10, pubkeyB2)
    val newBox2 = ErgoBox(10, pubkeyA2)

    val newBoxes = IndexedSeq(newBox1, newBox2)

    val properBytes = Bytes.concat(newBoxes.map(_.bytes): _*)

    val properHash = Blake2b256(properBytes)

    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    def mixingRequestProp(sender: ProveDlog, timeout: Int) = {
      val env = Map("sender" -> sender, "timeout" -> timeout, "properHash" -> properHash)
      val compiledProp = compile(env,
        """{
          |  let notTimePassed = HEIGHT <= timeout
          |  let outBytes = OUTPUTS.map(fun (box: Box) = box.bytes)
          |  let outSumBytes = outBytes.fold(EmptyByteArray, fun (arr1: ByteArray, arr2: ByteArray) = arr2 ++ arr1)
          |  let timePassed = HEIGHT > timeout
          |  notTimePassed && blake2b256(outSumBytes) == properHash || timePassed && sender
           }""".stripMargin).asBoolValue

      val prop = OR(
        AND(LE(Height, IntConstant(timeout)),
          EQ(CalcBlake2b256(Fold.sumBytes(MapCollection(Outputs, 21, ExtractBytes(TaggedBox(21))))),
            ByteArrayConstant(properHash))),
        AND(GT(Height, IntConstant(timeout)), sender)
      )
      compiledProp shouldBe prop
      compiledProp
    }

    val ctx = UtxoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = fakeSelf)

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

    val env = Map("pubkey" -> pubkey)
    val prop = compile(env,
      """{
        |  let outValues = OUTPUTS.map(fun (box: Box) = box.value)
        |  pubkey && outValues.fold(0, fun (x: Int, y: Int) = y + x) > 20
         }""".stripMargin).asBoolValue

    val propExp = AND(pubkey, GT(Fold.sum(MapCollection(Outputs, 21, ExtractAmount(TaggedBox(21)))), IntConstant(20)))
    prop shouldBe propExp

    val newBox1 = ErgoBox(11, pubkey)
    val newBox2 = ErgoBox(10, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val ctx = UtxoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage)
  }

  property("byindex") {
    val prover = new UtxoProvingInterpreter
    val verifier = new UtxoInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("pubkey" -> pubkey)
    val compiledProp = compile(env, """pubkey && OUTPUTS(0).value > 10""")

    val prop = AND(pubkey, GT(ExtractAmount(ByIndex(Outputs, 0)), IntConstant(10)))
    compiledProp shouldBe prop

    val newBox1 = ErgoBox(11, pubkey)
    val newBox2 = ErgoBox(10, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val ctx = UtxoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage)


    val fProp1 = AND(pubkey, GT(ExtractAmount(ByIndex(Outputs, 0)), IntConstant(11)))
    prover.prove(fProp1, ctx, fakeMessage).isSuccess shouldBe false

    val fProp2 = AND(pubkey, GT(ExtractAmount(ByIndex(Outputs, 1)), IntConstant(11)))
    prover.prove(fProp2, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("sizeof - num of outputs = num of inputs + 1") {
    val prover = new UtxoProvingInterpreter
    val verifier = new UtxoInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("pubkey" -> pubkey)
    val prop = compile(env, """pubkey && OUTPUTS.size == INPUTS.size + 1""").asBoolValue
    val propTree = AND(pubkey, EQ(SizeOf(Outputs), Plus(SizeOf(Inputs), IntConstant(1))))
    prop shouldBe propTree

    val newBox1 = ErgoBox(11, pubkey)
    val newBox2 = ErgoBox(10, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(21, pubkey)

    val ctx = UtxoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
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

    val prop = compile(Map(), "OUTPUTS.exists(fun (box: Box) = box.value + 5 > 10)").asBoolValue

    val expProp = Exists(Outputs, 21, GT(Plus(ExtractAmount(TaggedBox(21)), IntConstant(5)), IntConstant(10)))
    prop shouldBe expProp

    val newBox1 = ErgoBox(16, pubkey)
    val newBox2 = ErgoBox(15, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val ctx = UtxoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get shouldBe true
    //todo: finish
  }

  property("forall") {
    val prover = new UtxoProvingInterpreter
    val verifier = new UtxoInterpreter
    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = compile(Map(), "OUTPUTS.forall(fun (box: Box) = box.value == 10)").asBoolValue

    val propTree = ForAll(Outputs, 21, EQ(ExtractAmount(TaggedBox(21)), IntConstant(10)))
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey)
    val newBox2 = ErgoBox(10, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val ctx = UtxoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get shouldBe true
    //todo: finish
  }


  property("forall - fail") {
    val prover = new UtxoProvingInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = compile(Map(), "OUTPUTS.forall(fun (box: Box) = box.value == 10)").asBoolValue
    val propTree = ForAll(Outputs, 21, EQ(ExtractAmount(TaggedBox(21)), IntConstant(10)))
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey)
    val newBox2 = ErgoBox(11, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val ctx = UtxoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = fakeSelf)

    prover.prove(prop, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("counter") {
    val prover = new UtxoProvingInterpreter
    val verifier = new UtxoInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

//    val compiledProp = compile(Map(),
//      """OUTPUTS.exists(fun (box: Box) = {
//        |  box.R3[Int] == SELF.R3[Int] + 1
//         })""".stripMargin)

    val prop = Exists(Outputs, 21, EQ(ExtractRegisterAs(TaggedBox(21), R3)(SInt),
      Plus(ExtractRegisterAs(Self, R3)(SInt), IntConstant(1))))
//    compiledProp shouldBe prop

    val newBox1 = ErgoBox(10, pubkey, Map(R3 -> IntConstant(3)))
    val newBox2 = ErgoBox(10, pubkey, Map(R3 -> IntConstant(6)))
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(20, TrueLeaf, Map(R3 -> IntConstant(5)))

    val ctx = UtxoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = s)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get shouldBe true
  }

  property("counter - no register in outputs") {
    val prover = new UtxoProvingInterpreter
    val verifier = new UtxoInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

//    val compiledProp = compile(Map(),
//      """OUTPUTS.exists(fun (box: Box) = {
//        |  box.R3.getOrElse(0) == SELF.R3.get + 1
//         })""".stripMargin)

    val prop = Exists(Outputs, 21,
      EQ(ExtractRegisterAs(TaggedBox(21), R3, default = Some(IntConstant(0L))),
        Plus(ExtractRegisterAs(Self, R3), IntConstant(1))))
//    compiledProp shouldBe prop

    val newBox1 = ErgoBox(10, pubkey)
    val newBox2 = ErgoBox(10, pubkey, Map(R3 -> IntConstant(6)))
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(20, TrueLeaf, Map(R3 -> IntConstant(5)))

    val ctx = UtxoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = s)

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

//    val env = Map("key" -> key, "proof" -> proof)
//    val compiledProp = compile(env, """isMember(SELF.R3[AvlTree], key, proof)""")

    val prop = IsMember(ExtractRegisterAs(Self, R3),
      ByteArrayConstant(key),
      ByteArrayConstant(proof))
//    compiledProp shouldBe prop

    val newBox1 = ErgoBox(10, pubkey)
    val newBoxes = IndexedSeq(newBox1)

    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(20, TrueLeaf, Map(R3 -> AvlTreeConstant(treeData)))

    val ctx = UtxoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = s)

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

    val prover = new UtxoProvingInterpreter().withContextExtender(proofId, ByteArrayConstant(proof))
    val verifier = new UtxoInterpreter
    val pubkey = prover.dlogSecrets.head.publicImage

//    val env = Map("proofId" -> proofId)
//    val compiledProp = compile(env, """isMember(SELF.R3, SELF.R4, proofId)""")

    val prop = IsMember(ExtractRegisterAs(Self, R3), ExtractRegisterAs(Self, R4), TaggedByteArray(proofId))
//    compiledProp shouldBe prop

    val newBox1 = ErgoBox(10, pubkey)
    val newBoxes = IndexedSeq(newBox1)

    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(20, TrueLeaf, Map(R3 -> AvlTreeConstant(treeData), R4 -> ByteArrayConstant(key)))

    val ctx = UtxoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction, self = s)
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

//    val compiledProp = compile(Map(), """ProveDlog(SELF.R3) && ProveDlog(SELF.R4)""")

    val prop = AND(new ProveDlog(ExtractRegisterAs(Self, R3)), new ProveDlog(ExtractRegisterAs(Self, R4)))
//    compiledProp shouldBe prop

    val newBox1 = ErgoBox(10, pubkey3)
    val newBoxes = IndexedSeq(newBox1)
    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(20, TrueLeaf, Map(R3 -> pubkey1.value, R4 -> pubkey2.value))


    val ctx = UtxoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = s)
    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get shouldBe true

    //todo: check failing cases
  }

  /**
    * An example script where an output could be spent only along with an output with given id
    * (and no more outputs could be provided as an input of a spending transaction).
    */
  property("Along with a brother") {
    val prover = new UtxoProvingInterpreter
    val verifier = new UtxoInterpreter

    val pubkey1 = prover.dlogSecrets.head.publicImage
    val pubkey2 = prover.dlogSecrets(1).publicImage

    val brother = ErgoBox(10, pubkey1)

    val newBox = ErgoBox(20, pubkey2)

    val newBoxes = IndexedSeq(newBox)
    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val env = Map("brother" -> brother)
    val prop = compile(env,
      """{
        |  let okInputs = INPUTS.size == 2
        |  let okIds = INPUTS(0).id == brother.id
        |  okInputs && okIds
         }""".stripMargin).asBoolValue

    val propExpected = AND(
      EQ(SizeOf(Inputs), IntConstant(2)),
      EQ(ExtractId(ByIndex(Inputs, 0)), ExtractId(BoxConstant(brother))))
    prop shouldBe propExpected

    val s = ErgoBox(10, prop, Map())

    val ctx = UtxoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(brother, s),
      spendingTransaction,
      self = s)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get shouldBe true

    //todo: check failing branches
  }

  property("Not / If"){
    val prover = new UtxoProvingInterpreter
    val verifier = new UtxoInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val preimage = "hello world".getBytes("UTF-8")
    val preimageWrong = "wrong".getBytes("UTF-8")

    val hash = Blake2b256.hash(preimage)

    val prop = EQ(ByteArrayConstant(hash),
                  CalcBlake2b256(
                    If(Not(GT(ExtractAmount(ByIndex(Inputs, 0)), IntConstant(10))),
                      ExtractRegisterAs[SByteArray.type](ByIndex(Inputs, 1), R3),
                      ExtractRegisterAs[SByteArray.type](ByIndex(Inputs, 2), R3))))

    val input0 = ErgoBox(10, pubkey, Map())
    val input1 = ErgoBox(1, pubkey, Map(R3 -> ByteArrayConstant(preimage)))
    val input2 = ErgoBox(1, pubkey, Map(R3 -> ByteArrayConstant(preimageWrong)))
    val input3 = ErgoBox(10, prop, Map())

    val output = ErgoBox(22, pubkey, Map())

    val spendingTransaction = ErgoTransaction(IndexedSeq(), IndexedSeq(output))

    val ctx = UtxoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(input0, input1, input2, input3),
      spendingTransaction,
      self = input3)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get shouldBe true

    //todo: check failing branches
  }
}