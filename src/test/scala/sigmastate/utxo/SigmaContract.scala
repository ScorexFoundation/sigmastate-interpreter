package sigmastate.utxo

import sigmastate.lang.SigmaCompiler

abstract class SigmaContract {
  val compiler = new SigmaCompiler
}
