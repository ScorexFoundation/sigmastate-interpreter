package sigmastate

import scala.util.DynamicVariable

object Versions {
  /** Maximum version of ErgoTree supported by this interpreter release.
    * See version bits in `ErgoTree.header` for more details.
    * This value should be increased with each new protocol update via soft-fork.
    * The following values are used for current and upcoming forks:
    * - version 3.x this value must be 0
    * - in v4.0 must be 1
    * - in v5.x must be 2
    * etc.
    */
  val MaxSupportedScriptVersion: Byte = 2 // supported versions 0, 1, 2

  /** The first version of ErgoTree starting from which the JIT costing interpreter must be used.
    * It must also be used for all subsequent versions (3, 4, etc).
    */
  val JitActivationVersion: Byte = 2

  /** Universally accessible version of ErgoTree which is used to version the code
    * across the whole repository.
    *
    * The typical usage is to use `ergoTreeVersion.withValue(value) {...}` when the block
    * of code needs to be executed with the given version.
    *
    * For example, sigmastate.Interpreter uses it to execute operations according to the
    * necessary version if ErgoTree.
    */
  private val ergoTreeVersion: DynamicVariable[Byte] = new DynamicVariable[Byte](0)

  /** Returns version of ErgoTree executed by the current thread.
    * Each thread can have only one current version at any time, which can be changed using
    * withErgoTreeVersion method.
    *
    * @see withErgoTreeVersion
    */
  def currentErgoTreeVersion: Byte = ergoTreeVersion.value

  /** Executes the given block under the given version.
    * @param version ErgoTree version to be set on the current thread
    * @param block   block of code to execute
    * @return result of block execution
    */
  def withErgoTreeVersion[T](version: Byte)(block: => T): T =
    ergoTreeVersion.withValue(version)(block)
}
