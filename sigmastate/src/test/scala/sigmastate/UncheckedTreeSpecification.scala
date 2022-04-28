package sigmastate

import com.google.common.primitives.Ints
import org.ergoplatform.settings.ErgoAlgos
import org.scalacheck.Gen
import sigmastate.basics.DLogProtocol.{ProveDlog, SecondDLogProverMessage}
import sigmastate.basics.{SecondDiffieHellmanTupleProverMessage, ProveDHTuple}
import sigmastate.basics.VerifierMessage.Challenge
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.utils.Helpers

import java.math.BigInteger
import java.util.Arrays

class UncheckedTreeSpecification extends SigmaTestingCommons {
  property("Hash for UncheckedTree#challenge property should consider size of byte array"){
    val proveDlog = ProveDlog(Helpers.decodeECPoint("02e8e77123e300f8324e7b5c4cbe0f7ac616e0b78fc45f28f54fa6696231fc8ec3"))
    val secondDlog = SecondDLogProverMessage(BigInt("b277b8462a8b9098f5d4c934ab2876eb1b5707f3119e209bdbbad831e7cc4a41", 16))
    def uncheckedSchnorr(challenge: Challenge) = UncheckedSchnorr(proveDlog, None, challenge, secondDlog)
    
    val proveDHTuple =
      ProveDHTuple(
        Helpers.decodeECPoint("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"),
        Helpers.decodeECPoint("024ebfeb5a2b6ad997e40efb4888b3f091a611df8298cf7fb24315b4d112ad7c3c"),
        Helpers.decodeECPoint("03d41afc8c5875a8d52439d088b66ed63a5d64f16e1efd7f17c6036a923c637e5c"),
        Helpers.decodeECPoint("034132d4c7eb387f12ef40ba3ec03723bda0ee5707f7471185aafc316167e85137")
      )
    val secondDHTuple = SecondDiffieHellmanTupleProverMessage(new BigInteger("bb2e6f44a38052b3f564fafcd477c4eb8cda1a8a553a4a5f38f1e1084d6a69f0", 16))
    def uncheckedDiffieHellmanTuple(challenge: Challenge) = UncheckedDiffieHellmanTuple(proveDHTuple, None, challenge, secondDHTuple)

    def cAndUncheckedNode(challenge: Challenge) = CAndUncheckedNode(challenge, List.empty[UncheckedSigmaTree])
    def cOrUncheckedNode(challenge: Challenge) = COrUncheckedNode(challenge, List.empty[UncheckedSigmaTree])
    def cThresholdUncheckedNode(challenge: Challenge) = CThresholdUncheckedNode(challenge, List.empty[UncheckedSigmaTree], 0, None)

    val emptyChallenge = Challenge @@ Array.emptyByteArray
    val size3Challenge = Challenge @@ ErgoAlgos.decodeUnsafe("010203")
    val size4Challenge = Challenge @@ ErgoAlgos.decodeUnsafe("01020304")
    val bigChallenge = Challenge @@ ErgoAlgos.decodeUnsafe("c6429b70f4926a3ba1454f1aec116075f9e9fbe8a8f72114")
    val challenges = Array(emptyChallenge, size3Challenge, size4Challenge, bigChallenge)
    val constructors: Array[Challenge => UncheckedSigmaTree] = 
      Array(uncheckedSchnorr, uncheckedDiffieHellmanTuple, cAndUncheckedNode, cOrUncheckedNode, cThresholdUncheckedNode)
    
    val smallSizeExamples = for {
      challenge <- Gen.oneOf(Iterable(emptyChallenge, size3Challenge))
      f <- Gen.oneOf(constructors)
    } yield f(challenge)

    forAll(smallSizeExamples) { uncheckedTree =>
      uncheckedTree.challengeOptimizedHash shouldBe Arrays.hashCode(uncheckedTree.challenge)
    }

    val bigSizeExamples = for {
      challenge <- Gen.oneOf(Iterable(size4Challenge, bigChallenge))
      f <- Gen.oneOf(constructors)
    } yield f(challenge)

    forAll(bigSizeExamples) { uncheckedTree =>
      uncheckedTree.challengeOptimizedHash shouldBe Ints.fromByteArray(uncheckedTree.challenge)
    }

    val mixedExamples = for {
      challenge1 <- Gen.oneOf(challenges)
      challenge2 <- Gen.oneOf(challenges)
      f <- Gen.oneOf(constructors)
    } yield (f(challenge1), f(challenge2))

    forAll(mixedExamples) { case (uncheckedTree1, uncheckedTree2) =>
      if (uncheckedTree1 == uncheckedTree2) {
        uncheckedTree1.hashCode shouldBe uncheckedTree2.hashCode
      } else {
        uncheckedTree1.hashCode() should not be uncheckedTree2.hashCode()
      }
    }
  }
}
