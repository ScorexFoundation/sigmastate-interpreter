package sigmastate.eval

import java.lang.ref.WeakReference

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
  import ReallocatingIRContextManager._

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

object ReallocatingIRContextManager {
  private val _IR = new DynamicVariable[IRContext](null)
  private val _inProcess = new DynamicVariable(false)
}

/** An implementation of [[sigmastate.eval.IRContextManager]] which uses the given `irFactory`
  * to create a new IRContext instance once and then reset it when the given capacity is exceeded.
  * If capacity == UndefinedCapacity, the reset happens before EVERY execution.
  * The reset is done by [[scalan.Base#resetContext]] before `action` is executed.
  *
  * @param irFactory   factory to create new IRContext instances
  * @param capacity    if greater than 0 specifies a maximum number of the graph nodes in the context
  */
class ResettingIRContextManager(
    irFactory: IRContextFactory,
    capacity: Int = ResettingIRContextManager.UndefinedCapacity
  ) extends IRContextManager {
  import ResettingIRContextManager._

  private val _IR = new DynamicVariable[WeakReference[IRContext]](null)
  private val _inProcess = new DynamicVariable(false)

  /** Obtains the valid [[IRContext]] from thread local [[WeakReference]] (cached value).
    * Uses the `irFactory` to create a fresh new IRContext instance if necessary
    * if cached value is not available.
    * @return an instance of IRContext stored in the thread local cache
    */
  private def obtainIR: IRContext = {
    val currRef = _IR.value
    if (currRef == null) {
      // first access on the current thread, create both new IR and the ref
      val ir = irFactory.createIRContext
      _IR.value = new WeakReference(ir)
      ir
    } else {
      var ir = currRef.get()  // try resolving via weak reference
      if (ir == null) {
        // the reference was cleared, recreate
        println("the reference was cleared")
        ir = irFactory.createIRContext
        _IR.value = new WeakReference(ir)
      }
      ir
    }
  }

  override def executeWithIRContext[T](action: IRContext => T): T = {
    val currIR = obtainIR

    // reset the context when the capacity is not defined or the node counter is too high
    if (capacity == UndefinedCapacity || currIR.defCount > capacity) {
      currIR.resetContext()
    }

    if (_inProcess.value)
      sys.error(s"Nested executeWithContext is not supported")

    _inProcess.withValue(true) {
      action(currIR)
    }
  }
}

object ResettingIRContextManager {
  val UndefinedCapacity = 0
  val DefaultCapacity = 100000
}
