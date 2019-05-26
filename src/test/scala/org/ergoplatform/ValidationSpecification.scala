package org.ergoplatform

trait ValidationSpecification {
  implicit val vs: ValidationSettings = ValidationRules.currentSettings
}
