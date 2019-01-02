package sigmastate.utxo

import sigmastate.lang.{SigmaCompiler, TransformingSigmaBuilder}

abstract class SigmaContract {
  val compiler = new SigmaCompiler(TransformingSigmaBuilder, networkPrefix = None)
}
