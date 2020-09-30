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

/** An implementation of [[sigmastate.eval.IRContextManager]] which uses the given `irFactory`
  * to create a fresh new IRContext instance for every invocation of
  * [[sigmastate.eval.IRContextManager#executeWithIRContext]].
  */
class ReallocatingIRContextManager(irFactory: IRContextFactory) extends IRContextManager {
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

/** An implementation of [[sigmastate.eval.IRContextManager]] which uses the given `irFactory`
  * to create a new IRContext instance once and then on every invocation
  * [[scalan.Base#resetContext]] before `action` is executed..
  */
class ResettingIRContextManager(irFactory: IRContextFactory) extends IRContextManager {
  private val _IR = new DynamicVariable[IRContext](null)
  private val _inProcess = new DynamicVariable(false)

  override def executeWithIRContext[T](action: IRContext => T): T = {
    val currIR = _IR.value
    val preparedIR = if (currIR == null) {
      // first execution: create a new IR context
      val newIR = irFactory.createIRContext
      _IR.value = newIR
      newIR
    } else {
      // reset the context every time except the very first execution
      currIR.resetContext()
      currIR
    }

    if (_inProcess.value)
      sys.error(s"Nested executeWithContext is not supported")

    _inProcess.withValue(true) {
      action(preparedIR)
    }
  }
}
