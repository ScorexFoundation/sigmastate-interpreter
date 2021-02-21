package sigmastate.utxo

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import sigmastate.lang.{TransformingSigmaBuilder, SigmaCompiler, CompilerSettings}

abstract class SigmaContract {
  val compiler = new SigmaCompiler(CompilerSettings(
    TestnetNetworkPrefix, TransformingSigmaBuilder, lowerMethodCalls = true))
}
