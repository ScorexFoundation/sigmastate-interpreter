package org.ergoplatform.sdk.js

import sigma.data.Iso
import sigmastate.fleetSdkCommon.{distEsmTypesContextExtensionMod => contextExtensionMod}
import org.ergoplatform.sdk
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[org.ergoplatform.sdk.ReducedInputData]] available from JS. */
@JSExportTopLevel("ReducedInputData")
class ReducedInputData(
  val reductionResult: ReductionResult,
  val extension: contextExtensionMod.ContextExtension
) extends js.Object

@JSExportTopLevel("ReducedInputData$")
object ReducedInputData extends js.Object {
  val isoToSdk = new Iso[ReducedInputData, sdk.ReducedInputData] {
    override def to(a: ReducedInputData): sdk.ReducedInputData = {
      sdk.ReducedInputData(
        ReductionResult.isoToSdk.to(a.reductionResult),
        Isos.isoContextExtension.to(a.extension)
      )
    }

    override def from(b: sdk.ReducedInputData): ReducedInputData = {
      new ReducedInputData(
        ReductionResult.isoToSdk.from(b.reductionResult),
        Isos.isoContextExtension.from(b.extension)
      )
    }
  }
}