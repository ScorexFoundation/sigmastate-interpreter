package sigmastate.utxo

import org.ergoplatform.{ErgoLikeContext, ErgoLikeInterpreter}
import scorex.util.encode.Base16
import scorex.crypto.hash.Blake2b256
import sigmastate.Values._
import sigmastate._
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}
import sigmastate.lang.exceptions.OptionUnwrapNone

class ContextEnrichingSpecification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext
  property("context enriching mixed w. crypto") {
    val prover = new ErgoLikeProvingInterpreter
    val preimage = prover.contextExtenders(1).value.asInstanceOf[Array[Byte]]
    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("blake" -> Blake2b256(preimage), "pubkey" -> pubkey)
    val compiledScript = compile(env,
      """{
        |  pubkey && blake2b256(getVar[Array[Byte]](1).get) == blake
        |}
      """.stripMargin)
    val prop = BinAnd(
      pubkey.isValid,
      EQ(CalcBlake2b256(GetVarByteArray(1).get), ByteArrayConstant(Blake2b256(preimage)))
    )
    compiledScript shouldBe prop

    val ctx = ErgoLikeContext.dummy(fakeSelf)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeInterpreter
    verifier.verify(env, prop, ctx, pr.proof, fakeMessage).map(_._1).getOrElse(false) shouldBe false //context w/out extensions
    verifier.verify(env, prop, ctxv, pr.proof, fakeMessage).get._1 shouldBe true
  }

  ignore("context enriching mixed w. crypto 2") {
    val prover = new ErgoLikeProvingInterpreter
    val preimage1 = prover.contextExtenders(1).value.asInstanceOf[Array[Byte]]
    val preimage2 = prover.contextExtenders(2).value.asInstanceOf[Array[Byte]]
    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("blake" -> Blake2b256(preimage1 ++ preimage2), "pubkey" -> pubkey)
    val compiledScript = compile(env,
      """{
        |  pubkey && blake2b256(getVar[Array[Byte]](1).get ++ getVar[Array[Byte]](2).get) == blake
        |}
      """.stripMargin)

    val prop = BinAnd(
      pubkey.isValid,
      EQ(
        CalcBlake2b256(Append(GetVarByteArray(1).get, GetVarByteArray(2).get)),
        ByteArrayConstant(Blake2b256(preimage1 ++ preimage2))
      )
    )
    compiledScript shouldBe prop

    val ctx = ErgoLikeContext.dummy(fakeSelf)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeInterpreter
    verifier.verify(env, prop, ctx, pr.proof, fakeMessage).map(_._1).getOrElse(false) shouldBe false //context w/out extensions
    verifier.verify(env, prop, ctxv, pr.proof, fakeMessage).get._1 shouldBe true
  }

  ignore("prover enriching context - xor") {
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
        |  (getVar[Array[Byte]](k1).get | getVar[Array[Byte]](k2).get) == r
        |}
      """.stripMargin)

    val prop = EQ(Xor(GetVarByteArray(k1).get, GetVarByteArray(k2).get), ByteArrayConstant(r))
    compiledScript shouldBe prop

    val ctx = ErgoLikeContext.dummy(fakeSelf)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeInterpreter
    //context w/out extensions
    an[OptionUnwrapNone] should be thrownBy verifier.verify(env, prop, ctx, pr.proof, fakeMessage).get
    verifier.verify(env, prop, ctxv, pr.proof, fakeMessage).get._1 shouldBe true
  }

  /**
    * The script is asking for a hash function preimage. The "proof" could be replayed, so not really a proof.
    */
  ignore("prover enriching context(!!! ignored)") {
    val prover = new ErgoLikeProvingInterpreter
    val preimage = prover.contextExtenders(1: Byte).value.asInstanceOf[Array[Byte]]

    val env = Map("blake" -> Blake2b256(preimage))
    val compiledScript = compile(env,
      """{
        |  blake2b256(getVar[Array[Byte]](1).get) == blake
        |}
      """.stripMargin)

    val prop = EQ(CalcBlake2b256(GetVarByteArray(1).get), ByteArrayConstant(Blake2b256(preimage)))
    compiledScript shouldBe prop

    val ctx = ErgoLikeContext.dummy(fakeSelf)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeInterpreter
    //context w/out extensions
    assertExceptionThrown(verifier.verify(env, prop, ctx, pr.proof, fakeMessage).get,
      _.getCause.isInstanceOf[OptionUnwrapNone])
    verifier.verify(env, prop, ctxv, pr.proof, fakeMessage).get._1 shouldBe true
  }

  ignore("prover enriching context 2") {
    val prover = new ErgoLikeProvingInterpreter
    val preimage1 = prover.contextExtenders(1).value.asInstanceOf[Array[Byte]]
    val preimage2 = prover.contextExtenders(2).value.asInstanceOf[Array[Byte]]

    val env = Map("blake" -> Blake2b256(preimage2 ++ preimage1))
    val compiledScript = compile(env,
      """{
        |  blake2b256(getVar[Array[Byte]](2).get ++ getVar[Array[Byte]](1).get) == blake
        |}
      """.stripMargin)

    val prop = EQ(CalcBlake2b256(Append(GetVarByteArray(2).get, GetVarByteArray(1).get)),
      ByteArrayConstant(Blake2b256(preimage2 ++ preimage1)))
    compiledScript shouldBe prop

    val ctx = ErgoLikeContext.dummy(fakeSelf)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeInterpreter
    //context w/out extensions
    an[OptionUnwrapNone] should be thrownBy verifier.verify(env, prop, ctx, pr.proof, fakeMessage).get
    verifier.verify(env, prop, ctxv, pr.proof, fakeMessage).get._1 shouldBe true
  }
}
