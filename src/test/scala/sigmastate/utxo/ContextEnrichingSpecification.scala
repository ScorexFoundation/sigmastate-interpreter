package sigmastate.utxo

import org.ergoplatform.{ErgoLikeContext, ErgoLikeInterpreter}
import scorex.util.encode.Base16
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{ByteArrayConstant, TaggedByteArray}
import sigmastate._
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}

class ContextEnrichingSpecification extends SigmaTestingCommons {

  property("context enriching mixed w. crypto") {
    val prover = new ErgoLikeProvingInterpreter
    val preimage = prover.contextExtenders(1).value.asInstanceOf[Array[Byte]]
    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("blake" -> Blake2b256(preimage), "pubkey" -> pubkey)
    val compiledScript = compile(env,
      """{
        |  pubkey && blake2b256(getVar[Array[Byte]](1)) == blake
        |}
      """.stripMargin)
    val prop = AND(
      pubkey,
      EQ(CalcBlake2b256(TaggedByteArray(1)), ByteArrayConstant(Blake2b256(preimage)))
    )
    compiledScript shouldBe prop

    val ctx = ErgoLikeContext.dummy(fakeSelf)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeInterpreter
    verifier.verify(prop, ctx, pr.proof, fakeMessage).map(_._1).getOrElse(false) shouldBe false //context w/out extensions
    verifier.verify(prop, ctxv, pr.proof, fakeMessage).get._1 shouldBe true
  }

  property("context enriching mixed w. crypto 2") {
    val prover = new ErgoLikeProvingInterpreter
    val preimage1 = prover.contextExtenders(1).value.asInstanceOf[Array[Byte]]
    val preimage2 = prover.contextExtenders(2).value.asInstanceOf[Array[Byte]]
    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("blake" -> Blake2b256(preimage1 ++ preimage2), "pubkey" -> pubkey)
    val compiledScript = compile(env,
      """{
        |  pubkey && blake2b256(getVar[Array[Byte]](1) ++ getVar[Array[Byte]](2)) == blake
        |}
      """.stripMargin)

    val prop = AND(
      pubkey,
      EQ(
        CalcBlake2b256(Append(TaggedByteArray(1), TaggedByteArray(2))),
        ByteArrayConstant(Blake2b256(preimage1 ++ preimage2))
      )
    )
    compiledScript shouldBe prop

    val ctx = ErgoLikeContext.dummy(fakeSelf)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeInterpreter
    verifier.verify(prop, ctx, pr.proof, fakeMessage).map(_._1).getOrElse(false) shouldBe false //context w/out extensions
    verifier.verify(prop, ctxv, pr.proof, fakeMessage).get._1 shouldBe true
  }

  property("prover enriching context - xor") {
    val v1 = Base16.decode("abcdef7865").get
    val k1 = 21: Byte

    val v2 = Base16.decode("10abdca345").get
    val k2 = 22: Byte

    val r = Base16.decode("bb6633db20").get

    val prover = new ErgoLikeProvingInterpreter()
      .withContextExtender(k1, ByteArrayConstant(v1))
      .withContextExtender(k2, ByteArrayConstant(v2))

    val env = Map("k1" -> k1.toInt, "k2" -> k2.toInt, "r" -> r)
    val compiledScript = compile(env,
      """{
        |  (getVar[Array[Byte]](k1) | getVar[Array[Byte]](k2)) == r
        |}
      """.stripMargin)

    val prop = EQ(Xor(TaggedByteArray(k1), TaggedByteArray(k2)), ByteArrayConstant(r))
    compiledScript shouldBe prop

    val ctx = ErgoLikeContext.dummy(fakeSelf)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeInterpreter
    verifier.verify(prop, ctx, pr.proof, fakeMessage).get._1 shouldBe false //context w/out extensions
    verifier.verify(prop, ctxv, pr.proof, fakeMessage).get._1 shouldBe true
  }

  /**
    * The script is asking for a hash function preimage. The "proof" could be replayed, so not really a proof.
    */
  property("prover enriching context") {
    val prover = new ErgoLikeProvingInterpreter
    val preimage = prover.contextExtenders(1: Byte).value.asInstanceOf[Array[Byte]]

    val env = Map("blake" -> Blake2b256(preimage))
    val compiledScript = compile(env,
      """{
        |  blake2b256(getVar[Array[Byte]](1)) == blake
        |}
      """.stripMargin)

    val prop = EQ(CalcBlake2b256(TaggedByteArray(1)), ByteArrayConstant(Blake2b256(preimage)))
    compiledScript shouldBe prop

    val ctx = ErgoLikeContext.dummy(fakeSelf)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeInterpreter
    verifier.verify(prop, ctx, pr.proof, fakeMessage).get._1 shouldBe false //context w/out extensions
    verifier.verify(prop, ctxv, pr.proof, fakeMessage).get._1 shouldBe true
  }

  property("prover enriching context 2") {
    val prover = new ErgoLikeProvingInterpreter
    val preimage1 = prover.contextExtenders(1).value.asInstanceOf[Array[Byte]]
    val preimage2 = prover.contextExtenders(2).value.asInstanceOf[Array[Byte]]

    val env = Map("blake" -> Blake2b256(preimage2 ++ preimage1))
    val compiledScript = compile(env,
      """{
        |  blake2b256(getVar[Array[Byte]](2) ++ getVar[Array[Byte]](1)) == blake
        |}
      """.stripMargin)

    val prop = EQ(CalcBlake2b256(Append(TaggedByteArray(2), TaggedByteArray(1))),
      ByteArrayConstant(Blake2b256(preimage2 ++ preimage1)))
    compiledScript shouldBe prop

    val ctx = ErgoLikeContext.dummy(fakeSelf)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeInterpreter
    verifier.verify(prop, ctx, pr.proof, fakeMessage).get._1 shouldBe false //context w/out extensions
    verifier.verify(prop, ctxv, pr.proof, fakeMessage).get._1 shouldBe true
  }
}
