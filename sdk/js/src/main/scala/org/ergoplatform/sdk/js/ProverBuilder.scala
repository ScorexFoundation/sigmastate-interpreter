package org.ergoplatform.sdk.js

import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import org.ergoplatform.sdk.wallet.protocol.context.ErgoLikeParameters
import org.ergoplatform.sdk
import org.ergoplatform.sdk.SecretString

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import Isos._
import sigmastate.eval.SigmaDsl

@JSExportTopLevel("ProverBuilder")
class ProverBuilder(parameters: ErgoLikeParameters, networkPrefix: NetworkPrefix) extends js.Object {
  val _builder = new sdk.ProverBuilder(parameters, networkPrefix)

  def withMnemonic(mnemonicPhrase: String, mnemonicPass: String): ProverBuilder = {
    _builder.withMnemonic(
      SecretString.create(mnemonicPhrase),
      SecretString.create(mnemonicPass),
      usePre1627KeyDerivation = false
    )
    this
  }

  def withEip3Secret(index: Int): ProverBuilder = {
    _builder.withEip3Secret(index)
    this
  }

  def withDHTData(g: String, h: String, u: String, v: String, x: js.BigInt): ProverBuilder = {
    _builder.withDHTData(
      isoStringToGroupElement.to(g),
      isoStringToGroupElement.to(h),
      isoStringToGroupElement.to(u),
      isoStringToGroupElement.to(v),
      SigmaDsl.toBigInteger(isoBigInt.to(x))
    )
    this
  }

  def withDLogSecret(x: js.BigInt): ProverBuilder = {
    _builder.withDLogSecret(SigmaDsl.toBigInteger(isoBigInt.to(x)))
    this
  }

  def build(): Prover = {
    val p =_builder.build()
    new Prover(p)
  }
}
