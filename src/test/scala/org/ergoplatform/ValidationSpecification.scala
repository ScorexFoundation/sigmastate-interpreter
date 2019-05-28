package org.ergoplatform

import org.ergoplatform.validation.{SigmaValidationSettings, ValidationRules}

trait ValidationSpecification {
  implicit val vs: SigmaValidationSettings = ValidationRules.currentSettings
}
