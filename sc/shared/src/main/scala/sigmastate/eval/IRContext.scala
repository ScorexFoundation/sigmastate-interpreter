package sigmastate.eval

import sigma.ast.TransformingSigmaBuilder

/** Main interface of graph IR context which contain both GraphBuilding and TreeBuilding
  * methods.
  * It is not used in v5.0 interpreter and thus not part of consensus.
  *
  * Used in ErgoScript compiler only.
  *
  * @see CompiletimeIRContext
  */
trait IRContext extends TreeBuilding with GraphBuilding {
  override val builder = TransformingSigmaBuilder

  /** Pass configuration which is used to turn-off constant propagation.
    * USED IN TESTS ONLY.
    * @see `beginPass(noCostPropagationPass)`  */
  lazy val noConstPropagationPass = new DefaultPass(
    "noCostPropagationPass",
    Pass.defaultPassConfig.copy(constantPropagation = false))
}

/** IR context to be used by script development tools to compile ErgoScript into ErgoTree bytecode. */
class CompiletimeIRContext extends IRContext
