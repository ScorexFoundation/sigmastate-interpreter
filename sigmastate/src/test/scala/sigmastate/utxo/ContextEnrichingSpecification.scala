package sigmastate.utxo

import org.ergoplatform.ErgoLikeContext
import scorex.util.encode.Base16
import scorex.crypto.hash.Blake2b256
import sigmastate.Values._
import sigmastate._
import sigmastate.eval.IRContextFactoryImpl
import sigmastate.lang.Terms._
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, SigmaTestingCommons, ErgoLikeTestInterpreter}
import special.collection.Coll


class ContextEnrichingSpecification extends SigmaTestingCommons {

  implicit lazy val IR: TestingIRContext = new TestingIRContext
  implicit lazy val irFactory = new IRContextFactoryImpl(IR)

  property("context enriching mixed w. crypto") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val preimage = prover.contextExtenders(1).value.asInstanceOf[Coll[Byte]]
    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("blake" -> Blake2b256(preimage.toArray), "pubkey" -> pubkey)
    val compiledScript = compile(env,
      """{
        |  pubkey && blake2b256(getVar[Coll[Byte]](1).get) == blake
        |}
      """.stripMargin).asSigmaProp
    val prop = SigmaAnd(
      pubkey,
      EQ(CalcBlake2b256(GetVarByteArray(1).get), ByteArrayConstant(Blake2b256(preimage.toArray))).toSigmaProp
    )
    compiledScript shouldBe prop

    val ctx = ErgoLikeContextTesting.dummy(fakeSelf)
    val pr = prover.prove(compiledScript, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeTestInterpreter
    verifier.verify(env, compiledScript, ctx, pr.proof, fakeMessage).map(_._1).getOrElse(false) shouldBe false //context w/out extensions
    verifier.verify(env, compiledScript, ctxv, pr.proof, fakeMessage).get._1 shouldBe true
  }

  property("context enriching mixed w. crypto 2") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val preimage1 = prover.contextExtenders(1).value.asInstanceOf[Coll[Byte]]
    val preimage2 = prover.contextExtenders(2).value.asInstanceOf[Coll[Byte]]
    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("blake" -> Blake2b256(preimage1.append(preimage2).toArray), "pubkey" -> pubkey)
    val compiledScript = compile(env,
      """{
        |  pubkey && blake2b256(getVar[Coll[Byte]](1).get ++ getVar[Coll[Byte]](2).get) == blake
        |}
      """.stripMargin).asSigmaProp

    val prop = SigmaAnd(
      pubkey,
      EQ(
        CalcBlake2b256(Append(GetVarByteArray(1).get, GetVarByteArray(2).get)),
        ByteArrayConstant(Blake2b256(preimage1.append(preimage2).toArray))
      ).toSigmaProp
    )
    compiledScript shouldBe prop

    val ctx = ErgoLikeContextTesting.dummy(fakeSelf)
    val pr = prover.prove(compiledScript, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeTestInterpreter
    verifier.verify(env, compiledScript, ctx, pr.proof, fakeMessage).map(_._1).getOrElse(false) shouldBe false //context w/out extensions
    verifier.verify(env, compiledScript, ctxv, pr.proof, fakeMessage).get._1 shouldBe true
  }

  property("prover enriching context - xor") {
    val v1 = Base16.decode("abcdef7865").get
    val k1 = 21: Byte

    val v2 = Base16.decode("10abdca345").get
    val k2 = 22: Byte

    val r = Base16.decode("bb6633db20").get

    val prover = new ContextEnrichingTestProvingInterpreter()
      .withContextExtender(k1, ByteArrayConstant(v1))
      .withContextExtender(k2, ByteArrayConstant(v2))

    val env = Map("k1" -> k1.toInt, "k2" -> k2.toInt, "r" -> r)
    val compiledScript = compile(env,
      "{ xor(getVar[Coll[Byte]](k1).get, getVar[Coll[Byte]](k2).get) == r }").asBoolValue.toSigmaProp

    val prop = EQ(Xor(GetVarByteArray(k1).get, GetVarByteArray(k2).get), ByteArrayConstant(r)).toSigmaProp
    compiledScript shouldBe prop

    val ctx = ErgoLikeContextTesting.dummy(fakeSelf)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeTestInterpreter
    //context w/out extensions
    assertExceptionThrown(
      verifier.verify(env, prop, ctx, pr.proof, fakeMessage).get,
      rootCause(_).isInstanceOf[ArrayIndexOutOfBoundsException]
    )
    verifier.verify(env, prop, ctxv, pr.proof, fakeMessage).get._1 shouldBe true
  }

  /**
    * The script is asking for a hash function preimage. The "proof" could be replayed, so not really a proof.
    */
  property("prover enriching context") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val preimage = prover.contextExtenders(1).value.asInstanceOf[Coll[Byte]]

    val env = Map("blake" -> Blake2b256(preimage.toArray))
    val compiledScript = compile(env,
      """{
        |  blake2b256(getVar[Coll[Byte]](1).get) == blake
        |}
      """.stripMargin).asBoolValue.toSigmaProp

    val prop = EQ(CalcBlake2b256(GetVarByteArray(1).get), ByteArrayConstant(Blake2b256(preimage.toArray))).toSigmaProp
    compiledScript shouldBe prop

    val ctx = ErgoLikeContextTesting.dummy(fakeSelf)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeTestInterpreter
    //context w/out extensions
    assertExceptionThrown(verifier.verify(env, prop, ctx, pr.proof, fakeMessage).get,
      rootCause(_).isInstanceOf[ArrayIndexOutOfBoundsException])
    verifier.verify(env, prop, ctxv, pr.proof, fakeMessage).get._1 shouldBe true
  }

  property("prover enriching context 2") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val preimage1 = prover.contextExtenders(1).value.asInstanceOf[Coll[Byte]]
    val preimage2 = prover.contextExtenders(2).value.asInstanceOf[Coll[Byte]]

    val env = Map("blake" -> Blake2b256(preimage2.append(preimage1).toArray))
    val compiledScript = compile(env,
      """{
        |  blake2b256(getVar[Coll[Byte]](2).get ++ getVar[Coll[Byte]](1).get) == blake
        |}
      """.stripMargin).asBoolValue.toSigmaProp

    val prop = EQ(CalcBlake2b256(Append(GetVarByteArray(2).get, GetVarByteArray(1).get)),
      ByteArrayConstant(Blake2b256(preimage2.append(preimage1).toArray))).toSigmaProp
    compiledScript shouldBe prop

    val ctx = ErgoLikeContextTesting.dummy(fakeSelf)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeTestInterpreter
    //context w/out extensions
    assertExceptionThrown(
      verifier.verify(env, prop, ctx, pr.proof, fakeMessage).get,
      rootCause(_).isInstanceOf[ArrayIndexOutOfBoundsException]
    )
    verifier.verify(env, prop, ctxv, pr.proof, fakeMessage).get._1 shouldBe true
  }
}
