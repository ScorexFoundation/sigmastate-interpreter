package org.ergoplatform.sdk.js

import org.ergoplatform.sdk

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

trait JsWrapper[T] extends js.Object {
  def wrappedValue: T
}

@JSExportTopLevel("ContractTemplate")
class ContractTemplate(override val wrappedValue: sdk.ContractTemplate) extends JsWrapper[sdk.ContractTemplate] {
  /** @return JSON representation of this contract template pretty-printed to a string
    *         indentation of two spaces.
    */
  def toJsonString: String =
    sdk.ContractTemplate.jsonEncoder.encoder(wrappedValue).spaces2
}

@JSExportTopLevel("ContractTemplateObj")
object ContractTemplate {
  /** Create a new contract template from a JSON string.
    *
    * @param json JSON string representing a contract template.
    * @return a new contract template.
    */
  def fromJsonString(json: String): ContractTemplate = {
    io.circe.parser.parse(json) match {
      case Left(err) => throw err
      case Right(json) =>
        val ct = sdk.ContractTemplate.jsonEncoder.decoder(json.hcursor).right.get
        new ContractTemplate(ct)
    }
  }
}