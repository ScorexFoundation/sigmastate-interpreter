package sigmastate.interpreter

import sigmastate.serialization.ErgoTreeSerializer
import org.ergoplatform.settings.ErgoAlgos
import sigmastate.helpers.SigmaPPrint
import special.sigma.SigmaTestingData

class PredefScriptProcessorSpecification extends SigmaTestingData {

  property("deserialize from hex") {
    predefScriptHexes.foreach { hex =>
      val bytes = ErgoAlgos.decodeUnsafe(hex)
      val tree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bytes)
      println(
        s"""Tree: '$hex'
          |------------------------------------------
          |""".stripMargin)
      SigmaPPrint.pprintln(tree, width = 100, height = 150)
      println()
    }
  }

}
