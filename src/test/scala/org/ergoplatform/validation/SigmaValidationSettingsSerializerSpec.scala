package org.ergoplatform.validation

import org.scalatest.Assertion
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.serialization.SerializationSpecification

class SigmaValidationSettingsSerializerSpec extends SerializationSpecification with SigmaTestingCommons {

  private def roundtrip(settings: SigmaValidationSettings): Assertion = {
    implicit val set = SigmaValidationSettingsSerializer
    roundTripTest(settings)
    roundTripTestWithPos(settings)
  }

  property("ValidationRules.currentSettings round trip") {
    roundtrip(ValidationRules.currentSettings)
  }

  property("SigmaValidationSettings round trip") {
    forAll(ruleIdGen, statusGen, MinSuccessful(100)) { (ruleId, status) =>
      val vs = ValidationRules.currentSettings.updated(ruleId, status)
      roundtrip(vs)
    }
  }

}