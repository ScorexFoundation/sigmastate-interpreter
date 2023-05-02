package org.ergoplatform.validation

trait ValidationSpecification {
  implicit val vs: SigmaValidationSettings = ValidationRules.currentSettings
}
