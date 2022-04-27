package sigmastate

import com.google.common.primitives.Ints
import org.ergoplatform.settings.ErgoAlgos
import sigmastate.basics.DLogProtocol.{ProveDlog, SecondDLogProverMessage}
import sigmastate.basics.VerifierMessage.Challenge
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.utils.Helpers

import java.util.Arrays

class UncheckedTreeSpecification extends SigmaTestingCommons {
  property("UncheckedSchnorr hashCode"){
    def uncheckedSchnorr(challenge: Challenge) = UncheckedSchnorr(
      ProveDlog(Helpers.decodeECPoint("02e8e77123e300f8324e7b5c4cbe0f7ac616e0b78fc45f28f54fa6696231fc8ec3")),
      None,
      challenge,
      SecondDLogProverMessage(BigInt("b277b8462a8b9098f5d4c934ab2876eb1b5707f3119e209bdbbad831e7cc4a41", 16))
    )

    val emptyChallenge = Challenge @@ Array.emptyByteArray
    uncheckedSchnorr(emptyChallenge).challengeOptimizedHash shouldBe Arrays.hashCode(emptyChallenge)

    val smallChallenge = Challenge @@ ErgoAlgos.decodeUnsafe("01020304")
    uncheckedSchnorr(smallChallenge).challengeOptimizedHash shouldBe Arrays.hashCode(smallChallenge)

    val bigChallenge = Challenge @@ ErgoAlgos.decodeUnsafe("c6429b70f4926a3ba1454f1aec116075f9e9fbe8a8f72114")
    uncheckedSchnorr(bigChallenge).challengeOptimizedHash shouldBe Ints.fromByteArray(bigChallenge)
  }
}
