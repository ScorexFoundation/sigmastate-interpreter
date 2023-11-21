package sigma.interpreter.js

import sigma.data.{Iso, WrapperOf}
import sigma.js.Isos

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("SigmaPropProver")
class SigmaPropProver(override val wrappedValue: org.ergoplatform.SigmaPropProver)
    extends WrapperOf[org.ergoplatform.SigmaPropProver] {
}

@JSExportTopLevel("SigmaPropProverObj")
object SigmaPropProver {
  def withSecretes(secrets: js.Array[ProverSecret]): SigmaPropProver = {
    val privateInputs = Isos.isoArrayToIndexed(Iso.identityIso[ProverSecret]).to(secrets).map(_.wrappedValue)
    new SigmaPropProver(new org.ergoplatform.SigmaPropProver(privateInputs))
  }
}