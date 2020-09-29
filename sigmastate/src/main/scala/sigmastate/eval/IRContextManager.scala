package sigmastate.eval

import scala.util.DynamicVariable

/** Provides a capability to execute actions with a IRContext instance. */
abstract class IRContextManager {
  /** Executes the given function with the underlying context.
    * @param action function to be executed
    * @return the result returned by the action
    */
  def executeWithIRContext[T](action: IRContext => T): T
}

/** Default implementation of [[IRContextManager]] which uses the given `irFactory`
  * to create a new IRContext instance.
  * Implements IRContext recycling to free up memory as necessary when the IR grows
  * above the threshold.
  */
class DefaultIRContextManager(irFactory: IRContextFactory) extends IRContextManager {
  private val _IR = new DynamicVariable[IRContext](null)
  private val _inProcess = new DynamicVariable(false)

  override def executeWithIRContext[T](action: IRContext => T): T = {
    val newIR = irFactory.createIRContext
    if (newIR eq _IR.value)
      sys.error(s"Factory should return a fresh instance of IRContext on every call.")
    _IR.value = newIR

    if (_inProcess.value)
      sys.error(s"Nested executeWithContext is not supported")

    _inProcess.withValue(true) {
      action(newIR)
    }
  }
}
