package sigmastate.utxo

import org.ergoplatform.{ErgoBoxCandidate, ErgoLikeTransaction, _}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.serialization.generators.ValueGenerators

class SerializationRoundTripSpec extends PropSpec
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ValueGenerators
  with SigmaTestingCommons {

  property("ErgoBoxCandidate: Serializer round trip") {
    forAll { t: ErgoBoxCandidate => roundTripTest(t)(ErgoBoxCandidate.serializer) }
    forAll { t: ErgoBoxCandidate => roundTripTestWithPos(t)(ErgoBoxCandidate.serializer) }
  }

  property("ErgoBox: Serializer round trip") {
    forAll { t: ErgoBox => roundTripTest(t)(ErgoBox.sigmaSerializer) }
    forAll { t: ErgoBox => roundTripTestWithPos(t)(ErgoBox.sigmaSerializer) }
  }

  property("ContextExtension: Serializer round trip") {
    forAll { t: ContextExtension => roundTripTest(t)(ContextExtension.serializer) }
    forAll { t: ContextExtension => roundTripTestWithPos(t)(ContextExtension.serializer) }
  }

  property("SerializedProverResult: Serializer round trip") {
    forAll { t: ProverResult => roundTripTest(t)(ProverResult.serializer) }
    forAll { t: ProverResult => roundTripTestWithPos(t)(ProverResult.serializer) }
  }

  property("Input: Serializer round trip") {
    forAll { t: Input => roundTripTest(t)(Input.serializer) }
    forAll { t: Input => roundTripTestWithPos(t)(Input.serializer) }
  }
}
