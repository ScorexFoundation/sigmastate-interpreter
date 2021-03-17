package sigmastate.serialization

import java.math.BigInteger
import java.util

import org.ergoplatform.settings.ErgoAlgos
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Assertion
import sigmastate.Values.SigmaBoolean
import sigmastate._
import sigmastate.basics.DLogProtocol.{ProveDlog, SecondDLogProverMessage}
import sigmastate.basics.VerifierMessage.Challenge
import sigmastate.basics.{SecondDiffieHellmanTupleProverMessage, ProveDHTuple}
import sigmastate.helpers.{ErgoLikeTransactionTesting, ErgoLikeContextTesting, SigmaTestingCommons, ContextEnrichingTestProvingInterpreter}
import sigmastate.interpreter.Interpreter
import sigmastate.serialization.generators.ObjectGenerators
import sigmastate.utils.Helpers

import scala.util.Random

class SigSerializerSpecification extends SigmaTestingCommons
  with ObjectGenerators with CrossVersionProps {
  implicit lazy val IR = new TestingIRContext
  private lazy implicit val arbExprGen: Arbitrary[SigmaBoolean] = Arbitrary(exprTreeGen)

  private lazy val prover = new ContextEnrichingTestProvingInterpreter()

  private lazy val interpreterProveDlogGen: Gen[ProveDlog] =
    Gen.oneOf(prover.dlogSecrets.map(_.publicImage))

  private lazy val interpreterProveDHTGen =
    Gen.oneOf(prover.dhSecrets.map(_.publicImage))

  private def exprTreeNodeGen: Gen[SigmaBoolean] = for {
    left <- exprTreeGen
    right <- exprTreeGen
    third <- exprTreeGen
    node <- Gen.oneOf(
      COR(Array(left, right)),
      CAND(Array(left, right))
      // TODO v5.0: uncomment to test property("SigSerializer round trip")
      // CTHRESHOLD(2, Array(left, right, third))
    )
  } yield node

  private def exprTreeGen: Gen[SigmaBoolean] =
    Gen.oneOf(interpreterProveDlogGen, interpreterProveDHTGen, Gen.delay(exprTreeNodeGen))

  private def isEquivalent(expected: ProofTree, actual: ProofTree): Boolean = (expected, actual) match {
    case (NoProof, NoProof) => true
    case (dht1: UncheckedDiffieHellmanTuple, dht2: UncheckedDiffieHellmanTuple) =>
      // `firstMessageOpt` is not serialized
      dht1.copy(commitmentOpt = None) == dht2
    case (sch1: UncheckedSchnorr, sch2: UncheckedSchnorr) =>
      // `firstMessageOpt` is not serialized
      sch1.copy(commitmentOpt = None) == sch2
    case (conj1: UncheckedConjecture, conj2: UncheckedConjecture) =>
      util.Arrays.equals(conj1.challenge, conj2.challenge) &&
        conj1.children.zip(conj2.children).forall(t => isEquivalent(t._1, t._2))
    case _ => false
  }


  private def roundTrip(uncheckedTree: UncheckedTree, exp: SigmaBoolean): Assertion = {
    val bytes = SigSerializer.toBytes(uncheckedTree)
    val parsedUncheckedTree = SigSerializer.parseAndComputeChallenges(exp, bytes)
    isEquivalent(uncheckedTree, parsedUncheckedTree) shouldBe true
  }

  property("SigSerializer no proof round trip") {
    roundTrip(NoProof, TrivialProp.TrueProp)
  }

  property("SigSerializer round trip") {
    forAll(configParams = MinSuccessful(100)) { sb: SigmaBoolean =>
      val expr = sb.toSigmaProp
      val challenge = Array.fill(32)(Random.nextInt(100).toByte)

      val ctx = ErgoLikeContextTesting(
        currentHeight = 1,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = ErgoLikeContextTesting.dummyPubkey,
        boxesToSpend = IndexedSeq(fakeSelf),
        spendingTransaction = ErgoLikeTransactionTesting.dummy,
        self = fakeSelf, activatedVersionInTests)
        .withCostLimit(Long.MaxValue) // To avoid occasional cost limit exceptions which are irrelevant here

      try {
        // get sigma conjectures out of transformers
        val tree = mkTestErgoTree(expr)
        val prop = prover.fullReduction(tree, ctx, Interpreter.emptyEnv)._1

        val proof = prover.prove(tree, ctx, challenge).get.proof
        val proofTree = SigSerializer.parseAndComputeChallenges(prop, proof)
        roundTrip(proofTree, prop)
// uncomment to print more useful test cases
//        val testCase = ProofTestCase(prop, proof, proofTree)
//        println(
//          s"""-------------------------------
//            |${sigmastate.helpers.SigmaPPrint(testCase, width = 150, height = 150)}
//            |""".stripMargin)
      } catch {
        case t: Throwable =>
          t.printStackTrace()
          throw t
      }
    }
  }

  case class ProofTestCase(
    prop: SigmaBoolean,
    proof: Array[Byte],
    uncheckedTree: UncheckedTree
  )

  property("SigSerializer test vectors")  {
    val cases = Seq(
      ProofTestCase(
        prop = ProveDlog(
          Helpers.decodeECPoint("02e8e77123e300f8324e7b5c4cbe0f7ac616e0b78fc45f28f54fa6696231fc8ec3")
        ),
        proof = ErgoAlgos.decodeUnsafe(
          "c6429b70f4926a3ba1454f1aec116075f9e9fbe8a8f72114b277b8462a8b9098f5d4c934ab2876eb1b5707f3119e209bdbbad831e7cc4a41"
        ),
        uncheckedTree = UncheckedSchnorr(
          ProveDlog(
            Helpers.decodeECPoint("02e8e77123e300f8324e7b5c4cbe0f7ac616e0b78fc45f28f54fa6696231fc8ec3")
          ),
          None,
          Challenge @@ ErgoAlgos.decodeUnsafe("c6429b70f4926a3ba1454f1aec116075f9e9fbe8a8f72114"),
          SecondDLogProverMessage(
            BigInt("b277b8462a8b9098f5d4c934ab2876eb1b5707f3119e209bdbbad831e7cc4a41", 16)
          )
        )
      ),
      ProofTestCase(
        ProveDHTuple(
          Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
          Helpers.decodeECPoint("024ebfeb5a2b6ad997e40efb4888b3f091a611df8298cf7fb24315b4d112ad7c3c"),
          Helpers.decodeECPoint("03d41afc8c5875a8d52439d088b66ed63a5d64f16e1efd7f17c6036a923c637e5c"),
          Helpers.decodeECPoint("034132d4c7eb387f12ef40ba3ec03723bda0ee5707f7471185aafc316167e85137")
        ),
        ErgoAlgos.decodeUnsafe(
          "9ec740b57353cb2f6035bb1a481b0066b2fdc0406a6fa67ebb2e6f44a38052b3f564fafcd477c4eb8cda1a8a553a4a5f38f1e1084d6a69f0"
        ),
        UncheckedDiffieHellmanTuple(
          ProveDHTuple(
            Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
            Helpers.decodeECPoint("024ebfeb5a2b6ad997e40efb4888b3f091a611df8298cf7fb24315b4d112ad7c3c"),
            Helpers.decodeECPoint("03d41afc8c5875a8d52439d088b66ed63a5d64f16e1efd7f17c6036a923c637e5c"),
            Helpers.decodeECPoint("034132d4c7eb387f12ef40ba3ec03723bda0ee5707f7471185aafc316167e85137")
          ),
          None,
          Challenge @@ ErgoAlgos.decodeUnsafe("9ec740b57353cb2f6035bb1a481b0066b2fdc0406a6fa67e"),
          SecondDiffieHellmanTupleProverMessage(
            new BigInteger("bb2e6f44a38052b3f564fafcd477c4eb8cda1a8a553a4a5f38f1e1084d6a69f0", 16)
          )
        )
      ),
      ProofTestCase(
        CAND(
          List(
            ProveDlog(
              Helpers.decodeECPoint("03670a10fcf68531423e3aa8bdad2d755eb5363ac53068e80d44578861f80abef3")
            ),
            ProveDlog(
              Helpers.decodeECPoint("0249829d9ca70fa3974c1354d7d112390e07b826032c5a7c3bc39e56b3f480bb87")
            )
          )
        ),
        ErgoAlgos.decodeUnsafe(
          "a00b476899e583aefc18b237a7a70e73baace72aa533271a561d3432c347dcaec8975fdefb36389abe21656aadcfda0a0259681ce17bc47c9539ae1e7068292bb9646a9ffe4e11653495bd67588cfd6454d82cc455036e5b"
        ),
        CAndUncheckedNode(
          Challenge @@ ErgoAlgos.decodeUnsafe("a00b476899e583aefc18b237a7a70e73baace72aa533271a"),
          List(
            UncheckedSchnorr(
              ProveDlog(
                Helpers.decodeECPoint(
                  "03670a10fcf68531423e3aa8bdad2d755eb5363ac53068e80d44578861f80abef3"
                )
              ),
              None,
              Challenge @@ ErgoAlgos.decodeUnsafe("a00b476899e583aefc18b237a7a70e73baace72aa533271a"),
              SecondDLogProverMessage(
                BigInt("561d3432c347dcaec8975fdefb36389abe21656aadcfda0a0259681ce17bc47c", 16)
              )
            ),
            UncheckedSchnorr(
              ProveDlog(
                Helpers.decodeECPoint(
                  "0249829d9ca70fa3974c1354d7d112390e07b826032c5a7c3bc39e56b3f480bb87"
                )
              ),
              None,
              Challenge @@ ErgoAlgos.decodeUnsafe("a00b476899e583aefc18b237a7a70e73baace72aa533271a"),
              SecondDLogProverMessage(
                BigInt("9539ae1e7068292bb9646a9ffe4e11653495bd67588cfd6454d82cc455036e5b", 16)
              )
            )
          )
        )
      ),
      ProofTestCase(
        COR(
          List(
            ProveDlog(
              Helpers.decodeECPoint("0344789e3a797e713103f2a8edd673fac35e56d414c584e575aaa750f3e8728b5b")
            ),
            ProveDlog(
              Helpers.decodeECPoint("0249829d9ca70fa3974c1354d7d112390e07b826032c5a7c3bc39e56b3f480bb87")
            )
          )
        ),
        ErgoAlgos.decodeUnsafe(
          "c617e65a2ca62ac97bc33a33b76cb669622129ba0e094ad96287d97c2c6d6c8e48790d7c44961f7d958d59222ab4d7c814808a466a3e66e6f98e02d421757baa2842288b8d02787b5111db2e8924623790175e5bf27a2e4513e8eb196c22c8cf26a9d7b51cd7e386508db9c12b070d84"
        ),
        COrUncheckedNode(
          Challenge @@ ErgoAlgos.decodeUnsafe("c617e65a2ca62ac97bc33a33b76cb669622129ba0e094ad9"),
          List(
            UncheckedSchnorr(
              ProveDlog(
                Helpers.decodeECPoint(
                  "0344789e3a797e713103f2a8edd673fac35e56d414c584e575aaa750f3e8728b5b"
                )
              ),
              None,
              Challenge @@ ErgoAlgos.decodeUnsafe("6287d97c2c6d6c8e48790d7c44961f7d958d59222ab4d7c8"),
              SecondDLogProverMessage(
                BigInt("14808a466a3e66e6f98e02d421757baa2842288b8d02787b5111db2e89246237", 16)
              )
            ),
            UncheckedSchnorr(
              ProveDlog(
                Helpers.decodeECPoint(
                  "0249829d9ca70fa3974c1354d7d112390e07b826032c5a7c3bc39e56b3f480bb87"
                )
              ),
              None,
              Challenge @@ ErgoAlgos.decodeUnsafe("a4903f2600cb464733ba374ff3faa914f7ac709824bd9d11"),
              SecondDLogProverMessage(
                BigInt("90175e5bf27a2e4513e8eb196c22c8cf26a9d7b51cd7e386508db9c12b070d84", 16)
              )
            )
          )
        )
      ),
      ProofTestCase(
        COR(
          Array(
            ProveDHTuple(
              Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
              Helpers.decodeECPoint("03a5f4c3b8217557514df3df8537ca13f991b11538935b2ea407e8b24afcabe509"),
              Helpers.decodeECPoint("029837d12c86c29c92e74229dfd3fcb10933b696685209b14baa74dbabacb2dee5"),
              Helpers.decodeECPoint("03f17cefec3911966dc9952090325267a5cf7f9b0be76b02623021989d7f0007a2")
            ),
            COR(
              Array(
                ProveDlog(Helpers.decodeECPoint("03f997167c03aa234732e3a68126b371dffa1e409f62ca8fa18cea6acd1dbe54d5")),
                ProveDHTuple(
                  Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
                  Helpers.decodeECPoint("03f5921dde02233135665d006838fcb783deca634ee333c5541cc05a9012e684ee"),
                  Helpers.decodeECPoint("039b65625db7aad6d86599355b7cac785e6b5ac85b8a32e0d6927b324704d0a261"),
                  Helpers.decodeECPoint("02fc58b939b105231da101540c87e56f5703460c179935aaee47137f3c367904f1")
                )
              )
            )
          )
        ),
        ErgoAlgos.decodeUnsafe(
          "96addfddcc197bdbacf5c0142fb16c39384b3699fa47da7dffd3149193b042fda134c0e208fefcb791379959ac6fc731adf47e32000fc75e2923dba482c843c7f6b684cbf2ceec5bfdf5fe6d13cabe5d15f8295ca4e8094fba3c4716bfdfc3c462417a79a61fcc487d6997a42739d533eebffa3b420a6e2e44616a1341e5baa1165c6c22e91a81addd97c3bd2fe40ecdbbda6f43bf71240da8dac878c044c16d42a4b34c536bbb1b"
        ),
        COrUncheckedNode(
          Challenge @@ ErgoAlgos.decodeUnsafe("96addfddcc197bdbacf5c0142fb16c39384b3699fa47da7d"),
          List(
            UncheckedDiffieHellmanTuple(
              ProveDHTuple(
                Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
                Helpers.decodeECPoint("03a5f4c3b8217557514df3df8537ca13f991b11538935b2ea407e8b24afcabe509"),
                Helpers.decodeECPoint("029837d12c86c29c92e74229dfd3fcb10933b696685209b14baa74dbabacb2dee5"),
                Helpers.decodeECPoint("03f17cefec3911966dc9952090325267a5cf7f9b0be76b02623021989d7f0007a2")
              ),
              None,
              Challenge @@ ErgoAlgos.decodeUnsafe("ffd3149193b042fda134c0e208fefcb791379959ac6fc731"),
              SecondDiffieHellmanTupleProverMessage(new BigInteger("adf47e32000fc75e2923dba482c843c7f6b684cbf2ceec5bfdf5fe6d13cabe5d", 16))
            ),
            COrUncheckedNode(
              Challenge @@ ErgoAlgos.decodeUnsafe("697ecb4c5fa939260dc100f6274f908ea97cafc056281d4c"),
              List(
                UncheckedSchnorr(
                  ProveDlog(Helpers.decodeECPoint("03f997167c03aa234732e3a68126b371dffa1e409f62ca8fa18cea6acd1dbe54d5")),
                  None,
                  Challenge @@ ErgoAlgos.decodeUnsafe("15f8295ca4e8094fba3c4716bfdfc3c462417a79a61fcc48"),
                  SecondDLogProverMessage(BigInt("7d6997a42739d533eebffa3b420a6e2e44616a1341e5baa1165c6c22e91a81ad", 16))
                ),
                UncheckedDiffieHellmanTuple(
                  ProveDHTuple(
                    Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
                    Helpers.decodeECPoint("03f5921dde02233135665d006838fcb783deca634ee333c5541cc05a9012e684ee"),
                    Helpers.decodeECPoint("039b65625db7aad6d86599355b7cac785e6b5ac85b8a32e0d6927b324704d0a261"),
                    Helpers.decodeECPoint("02fc58b939b105231da101540c87e56f5703460c179935aaee47137f3c367904f1")
                  ),
                  None,
                  Challenge @@ ErgoAlgos.decodeUnsafe("7c86e210fb413069b7fd47e09890534acb3dd5b9f037d104"),
                  SecondDiffieHellmanTupleProverMessage(new BigInteger("dd97c3bd2fe40ecdbbda6f43bf71240da8dac878c044c16d42a4b34c536bbb1b", 16))
                )
              )
            )
          )
        )
      ),
      ProofTestCase(
        COR(
          Array(
            CAND(
              Array(
                ProveDlog(Helpers.decodeECPoint("0368c0d88d9eb2972bbfc23c961de6307f6a944352cbfe316f262401feabdaa87d")),
                ProveDHTuple(
                  Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
                  Helpers.decodeECPoint("02badd523c2f5c12f4a3d4d667efb6ce8e95c8ad007cc697c34e91884335b55249"),
                  Helpers.decodeECPoint("02c455e55dc7bc731c5a487778e84814080fb70cc59957bc2a40c45373fe1ce14c"),
                  Helpers.decodeECPoint("029d4ec275379f9212a53e15994aef203dcec43a177c0b1f40afcf592e5753ce67")
                )
              )
            ),
            COR(
              Array(
                ProveDHTuple(
                  Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
                  Helpers.decodeECPoint("02a5af61e5c0eaad47ffdf29a91afffb1791295fff434831802e7f36b885fc2aa7"),
                  Helpers.decodeECPoint("03a9aa914199bb0e3b00ff4dd6ff82ec34d5f451825c28c41dc6432c763b6061e2"),
                  Helpers.decodeECPoint("0315d84dba1b29074f766e57bb11843687da899180cf2487ccecd0a3ec5f05365a")
                ),
                ProveDHTuple(
                  Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
                  Helpers.decodeECPoint("02a5af61e5c0eaad47ffdf29a91afffb1791295fff434831802e7f36b885fc2aa7"),
                  Helpers.decodeECPoint("03a9aa914199bb0e3b00ff4dd6ff82ec34d5f451825c28c41dc6432c763b6061e2"),
                  Helpers.decodeECPoint("0315d84dba1b29074f766e57bb11843687da899180cf2487ccecd0a3ec5f05365a")
                )
              )
            )
          )
        ),
        ErgoAlgos.decodeUnsafe(
          "4fdc76711fd844de0831d8e90ebaf9c622117a062b2f8b63ff8b9c2a4eed345a11c697f6850cf3a38763d738539ad2d2e0a3e44384f23eee260931d88e1f5241a2600a7c98545ada675fd5e627e8e84f140fc95e28775cde52e71bb4d7b5ee2564553fac5b52202530fcbcdf205b7cca145202fb2a5bb181a890eb15536b08b747ea163f6b5d32a116fa9e1eb6b348fd82d3ebc11c125e5bc3f09c499aa0a8db14dc1780b4181f9bae5ed0f743f71b82b18784380814507d810cbef61ebc0b30e7f324083e2d3d08"
        ),
        COrUncheckedNode(
          Challenge @@ ErgoAlgos.decodeUnsafe("4fdc76711fd844de0831d8e90ebaf9c622117a062b2f8b63"),
          List(
            CAndUncheckedNode(
              Challenge @@ ErgoAlgos.decodeUnsafe("ff8b9c2a4eed345a11c697f6850cf3a38763d738539ad2d2"),
              List(
                UncheckedSchnorr(
                  ProveDlog(Helpers.decodeECPoint("0368c0d88d9eb2972bbfc23c961de6307f6a944352cbfe316f262401feabdaa87d")),
                  None,
                  Challenge @@ ErgoAlgos.decodeUnsafe("ff8b9c2a4eed345a11c697f6850cf3a38763d738539ad2d2"),
                  SecondDLogProverMessage(BigInt("e0a3e44384f23eee260931d88e1f5241a2600a7c98545ada675fd5e627e8e84f", 16))
                ),
                UncheckedDiffieHellmanTuple(
                  ProveDHTuple(
                    Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
                    Helpers.decodeECPoint("02badd523c2f5c12f4a3d4d667efb6ce8e95c8ad007cc697c34e91884335b55249"),
                    Helpers.decodeECPoint("02c455e55dc7bc731c5a487778e84814080fb70cc59957bc2a40c45373fe1ce14c"),
                    Helpers.decodeECPoint("029d4ec275379f9212a53e15994aef203dcec43a177c0b1f40afcf592e5753ce67")
                  ),
                  None,
                  Challenge @@ ErgoAlgos.decodeUnsafe("ff8b9c2a4eed345a11c697f6850cf3a38763d738539ad2d2"),
                  SecondDiffieHellmanTupleProverMessage(new BigInteger("140fc95e28775cde52e71bb4d7b5ee2564553fac5b52202530fcbcdf205b7cca", 16))
                )
              )
            ),
            COrUncheckedNode(
              Challenge @@ ErgoAlgos.decodeUnsafe("b057ea5b5135708419f74f1f8bb60a65a572ad3e78b559b1"),
              List(
                UncheckedDiffieHellmanTuple(
                  ProveDHTuple(
                    Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
                    Helpers.decodeECPoint("02a5af61e5c0eaad47ffdf29a91afffb1791295fff434831802e7f36b885fc2aa7"),
                    Helpers.decodeECPoint("03a9aa914199bb0e3b00ff4dd6ff82ec34d5f451825c28c41dc6432c763b6061e2"),
                    Helpers.decodeECPoint("0315d84dba1b29074f766e57bb11843687da899180cf2487ccecd0a3ec5f05365a")
                  ),
                  None,
                  Challenge @@ ErgoAlgos.decodeUnsafe("145202fb2a5bb181a890eb15536b08b747ea163f6b5d32a1"),
                  SecondDiffieHellmanTupleProverMessage(new BigInteger("16fa9e1eb6b348fd82d3ebc11c125e5bc3f09c499aa0a8db14dc1780b4181f9b", 16))
                ),
                UncheckedDiffieHellmanTuple(
                  ProveDHTuple(
                    Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
                    Helpers.decodeECPoint("02a5af61e5c0eaad47ffdf29a91afffb1791295fff434831802e7f36b885fc2aa7"),
                    Helpers.decodeECPoint("03a9aa914199bb0e3b00ff4dd6ff82ec34d5f451825c28c41dc6432c763b6061e2"),
                    Helpers.decodeECPoint("0315d84dba1b29074f766e57bb11843687da899180cf2487ccecd0a3ec5f05365a")
                  ),
                  None,
                  Challenge @@ ErgoAlgos.decodeUnsafe("a405e8a07b6ec105b167a40ad8dd02d2e298bb0113e86b10"),
                  SecondDiffieHellmanTupleProverMessage(new BigInteger("ae5ed0f743f71b82b18784380814507d810cbef61ebc0b30e7f324083e2d3d08", 16))
                )
              )
            )
          )
        )
      )
    )

    cases.foreach { c =>
      val sigBytes = SigSerializer.toBytes(c.uncheckedTree)
      sigBytes shouldBe c.proof
      val uncheckedTree = SigSerializer.parseAndComputeChallenges(c.prop, c.proof)
      uncheckedTree shouldBe c.uncheckedTree
    }
  }
}
