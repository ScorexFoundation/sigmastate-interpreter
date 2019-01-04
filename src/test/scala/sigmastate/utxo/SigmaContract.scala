package sigmastate.utxo

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import sigmastate.lang.{SigmaCompiler, TransformingSigmaBuilder}

abstract class SigmaContract {
  val compiler = new SigmaCompiler(TransformingSigmaBuilder, TestnetNetworkPrefix)
}
